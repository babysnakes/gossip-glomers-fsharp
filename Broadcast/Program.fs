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

type BroadcastRepeaterMsg =
    { MsgId: int option
      Message: int
      Dispatcher: Dispatcher<Message>
      Nodes: string list }

type State =
    { Tick: int
      Messages: Set<int>
      Destinations: string list }

type StateAction =
    | SetMessages of Set<int>
    | SetDestinations of string list
    | State

type BroadcastRepeat = BroadcastRepeat of BroadcastRepeaterMsg
type StateMessage = StateAction * AsyncReplyChannel<State>

type StateManagement() =

    static let mutable _state =
        { Tick = 1
          Messages = Set.empty
          Destinations = [] }

    static let agent =
        MailboxProcessor<StateMessage>.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg, replyChannel = inbox.Receive()

                    match msg with
                    | SetMessages(messages) ->
                        _state <-
                            { _state with
                                Tick = _state.Tick + 1
                                Messages = messages }

                        replyChannel.Reply _state
                    | SetDestinations destinations ->
                        _state <-
                            { _state with
                                Tick = _state.Tick + 1
                                Destinations = destinations }

                        replyChannel.Reply _state
                    | State ->
                        _state <- { _state with Tick = _state.Tick + 1 }
                        replyChannel.Reply _state

                    return! loop ()
                }

            loop ())

    /// A helper to avoid waiting for reply when updating state
    static let postWithoutReply =
        MailboxProcessor<Set<int>>.Start(fun inbox ->
            let rec messageLoop () =
                async {
                    let! messages = inbox.Receive()
                    do agent.PostAndReply(fun rc -> SetMessages(messages), rc) |> ignore
                    return! messageLoop ()
                }

            messageLoop ())

    static member UpdateMessages(messages: Set<int>) = postWithoutReply.Post messages

    static member UpdateDestinations(destinations: string list) =
        agent.PostAndReply(fun rc -> SetDestinations(destinations), rc) |> ignore

    static member GetState() = agent.PostAndReply(fun rc -> State, rc)

let compareAndSyncMessages =
    MailboxProcessor<System.Tuple<Set<int>, string, Dispatcher<Message>>>.Start(fun inbox ->
        let rec loop count =
            async {
                let! messages, dest, dispatch = inbox.Receive()
                let state = StateManagement.GetState()
                let missing = Set.difference state.Messages messages

                for msg in missing do
                    { Message.empty () with
                        Typ = Broadcast
                        MsgId = Some(count)
                        Message = Some(msg) }
                    |> dispatch dest

                return! loop (count + 1)
            }

        loop 1)

let addInReply (msg: Message) : Message = { msg with InReplyTo = msg.MsgId }

let agent (dispatch: Dispatcher<Message>) : unit =
    let state = StateManagement.GetState()

    for dest in state.Destinations do
        { Typ = Read
          MsgId = Some(state.Tick)
          InReplyTo = None
          Message = None
          Messages = None
          Topology = None }
        |> dispatch dest

let handler (messages: Set<int>) (node: Node) (dispatch: Dispatcher<Message>) (MessageData(src, msg)) =
    match msg.Typ with
    | TopologyOk -> failwith "node received topology_ok"
    | ReadOk ->
        compareAndSyncMessages.Post(msg.Messages.Value |> Set.ofList, src, dispatch)
        messages
    | BroadcastOk -> messages
    | Broadcast ->
        let newMessages = Set.add msg.Message.Value messages

        { msg with
            Typ = BroadcastOk
            Message = None }
        |> addInReply
        |> dispatch src

        StateManagement.UpdateMessages newMessages
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
        |> Option.iter StateManagement.UpdateDestinations

        { msg with
            Typ = TopologyOk
            Topology = None }
        |> addInReply
        |> dispatch src

        messages

let agentData = { Agent = agent; Frequency = 500 }

Node.run (Some(agentData)) { Handler = handler; State = Set.empty }
