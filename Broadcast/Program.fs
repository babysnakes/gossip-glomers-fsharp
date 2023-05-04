open FSharp.Json
open GGF.Lib
open Microsoft.FSharp.Core

type RPCType =
    | [<JsonUnionCase("broadcast")>] Broadcast
    | [<JsonUnionCase("broadcast_ok")>] BroadcastOk
    | [<JsonUnionCase("read")>] Read
    | [<JsonUnionCase("read_ok")>] ReadOk
    | [<JsonUnionCase("topology")>] Topology
    | [<JsonUnionCase("topology_ok")>] TopologyOk
    | [<JsonUnionCase("gossip")>] Gossip
    | [<JsonUnionCase("gossip_ok")>] GossipOk

type Nodes = Map<string, string list>

type Message =
    { [<JsonField("type")>]
      Typ: RPCType
      [<JsonField("msg_id")>]
      MsgId: int option
      [<JsonField("in_reply_to")>]
      InReplyTo: int option
      [<JsonField("message")>]
      Message: int option
      [<JsonField("messages")>]
      Messages: Option<int list>
      [<JsonField("topology")>]
      Topology: Nodes option }

    static member empty() =
        { Typ = Broadcast
          MsgId = None
          InReplyTo = None
          Message = None
          Messages = None
          Topology = None }

type State =
    { Tick: int
      Messages: Set<int>
      Destinations: string list
      Sent: Map<string, Set<int>> }

type StateAction =
    | SetMessages of Set<int>
    | SetDestinations of string list
    | UpdateSent of string * Set<int>
    | State of AsyncReplyChannel<State>

[<RequireQualifiedAccess>]
module StateManagement =

    let private defaultState =
        { Tick = 1
          Messages = Set.empty
          Destinations = []
          Sent = Map.empty }

    let private agent =
        MailboxProcessor<StateAction>.Start(fun inbox ->
            let rec loop (state: State) =
                async {
                    let! msg = inbox.Receive()

                    let newState =
                        match msg with
                        | SetMessages(messages) -> { state with Messages = messages }
                        | SetDestinations destinations ->
                            { state with
                                Destinations = destinations }
                        | UpdateSent(src, known) ->
                            let updated =
                                state.Sent
                                |> Map.tryFind src
                                |> Option.defaultValue Set.empty
                                |> Set.union known

                            { state with
                                Sent = Map.add src updated state.Sent }
                        | State replyChannel ->
                            replyChannel.Reply state
                            state

                    return!
                        loop
                            { newState with
                                Tick = newState.Tick + 1 }
                }

            loop defaultState)

    let updateMessages (messages: Set<int>) = agent.Post(SetMessages(messages))

    let updateSent (dest: string) (messages: Set<int>) = agent.Post(UpdateSent(dest, messages))

    let updateDestinations (destinations: string list) =
        agent.Post(SetDestinations(destinations))

    let getState () = agent.PostAndReply(fun rc -> State(rc))

let addInReply (msg: Message) : Message = { msg with InReplyTo = msg.MsgId }

let agent (dispatch: Dispatcher<Message>) : unit =
    let state = StateManagement.getState ()

    let computeMessages dest =
        state.Sent
        |> Map.tryFind dest
        |> Option.defaultValue Set.empty
        |> Set.difference state.Messages
        |> Set.toList
        |> Some

    for dest in state.Destinations do
        { Message.empty () with
            MsgId = Some(state.Tick)
            Typ = Gossip
            Messages = computeMessages dest }
        |> dispatch dest

let handler (messages: Set<int>) (node: Node) (dispatch: Dispatcher<Message>) (MessageData(src, msg)) =
    match msg.Typ with
    | ReadOk
    | TopologyOk -> failwith $"node received {msg.Typ}"
    | BroadcastOk -> messages
    | Gossip ->
        let newMessages = msg.Messages.Value |> Set.ofList |> Set.union messages
        StateManagement.updateMessages newMessages
        newMessages
    | GossipOk ->
        msg.Messages.Value
        |> Set.ofList
        |> StateManagement.updateSent src
        
        messages
    | Broadcast ->
        let newMessages = Set.add msg.Message.Value messages

        { msg with
            Typ = BroadcastOk
            Message = None }
        |> addInReply
        |> dispatch src

        StateManagement.updateMessages newMessages
        newMessages
    | Read ->
        { msg with
            Typ = ReadOk
            Messages = Some(messages |> Set.toList) }
        |> addInReply
        |> dispatch src

        messages
    | Topology ->
        msg.Topology
        |> Option.map (Map.tryFind node.Name >> Option.defaultValue [])
        |> Option.iter StateManagement.updateDestinations

        { msg with
            Typ = TopologyOk
            Topology = None }
        |> addInReply
        |> dispatch src

        messages

let agentData = { Agent = agent; Frequency = 100 }

Node.run (Some(agentData)) { Handler = handler; State = Set.empty }
