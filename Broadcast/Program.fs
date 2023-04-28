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

type BroadcastRepeaterMsg =
    { MsgId: int option
      Message: int
      Dispatcher: Dispatcher<Message>
      Nodes: string list }

type State =
    { Messages: Set<int>
      Destinations: string list }

type BroadcastRepeat = BroadcastRepeat of BroadcastRepeaterMsg

let broadcastRepeaterAgent =
    MailboxProcessor<BroadcastRepeat>.Start(fun inbox ->
        let rec messageLoop () =
            async {
                let! BroadcastRepeat(msg) = inbox.Receive()

                do
                    msg.Nodes
                    |> List.iter (fun dest ->
                        { MsgId = msg.MsgId
                          Message = Some(msg.Message)
                          Typ = Broadcast
                          InReplyTo = None
                          Messages = None
                          Topology = None }
                        |> msg.Dispatcher dest)

                return! messageLoop ()
            }

        messageLoop ())

let addInReply (msg: Message) : Message = { msg with InReplyTo = msg.MsgId }

let handler (state: State) (node: Node) (dispatch: Dispatcher<Message>) (MessageWithSource(src, msg)) =
    match msg.Typ with
    | ReadOk
    | TopologyOk -> failwith "node received OK type"
    | BroadcastOk -> state
    | Broadcast ->
        let newState =
            { state with
                Messages = Set.add msg.Message.Value state.Messages }

        { MsgId = msg.MsgId
          Message = msg.Message.Value
          Dispatcher =  dispatch
          Nodes = node.Neighbours }
        |> BroadcastRepeat
        |> broadcastRepeaterAgent.Post

        { msg with
            Typ = BroadcastOk
            Message = None }
        |> addInReply
        |> dispatch src

        newState
    | Read ->
        { msg with
            Typ = ReadOk
            Messages = Some(state.Messages |> Set.toList) }
        |> addInReply
        |> dispatch src

        state
    | Topology ->
        let newState =
            msg.Topology
            |> Option.map (Map.tryFind node.Name >> Option.defaultValue [])
            |> function
                | Some(destinations) ->
                    { state with
                        Destinations = destinations }
                | None -> state

        { msg with
            Typ = TopologyOk
            Topology = None }
        |> addInReply
        |> dispatch src

        newState

let state =
    { Messages = Set.empty
      Destinations = [] }

Node.run state handler
