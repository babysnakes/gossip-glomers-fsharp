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
      Node: string
      Nodes: string list }

type State = { mutable Messages: int list }

type BroadcastRepeat = BroadcastRepeat of BroadcastRepeaterMsg

let broadcastRepeaterAgent =
    MailboxProcessor<BroadcastRepeat>.Start(fun inbox ->
        let rec messageLoop () =
            async {
                let! BroadcastRepeat(msg) = inbox.Receive()

                msg.Nodes
                |> List.iter (fun node ->
                    { Src = msg.Node
                      Dst = node
                      Body =
                        { MsgId = msg.MsgId
                          Message = Some(msg.Message)
                          Typ = Broadcast
                          InReplyTo = None
                          Messages = None
                          Topology = None } }
                    |> Json.serializeEx Node.jsonOptions
                    |> Node.send)

                return! messageLoop ()
            }

        messageLoop ())

let addInReply (msg: Message) : Message = { msg with InReplyTo = msg.MsgId }

let handler (state: State) (node: Node) (msg: Message) : Message option =
    match msg.Typ with
    | ReadOk
    | TopologyOk -> failwith "node received OK type"
    | BroadcastOk -> None
    | Broadcast ->
        state.Messages <- msg.Message.Value :: state.Messages

        { MsgId = msg.MsgId
          Message = msg.Message.Value
          Node = node.Name
          Nodes = node.Neighbours }
        |> BroadcastRepeat
        |> broadcastRepeaterAgent.Post

        { msg with
            Typ = BroadcastOk
            Message = None }
        |> addInReply
        |> Some
    | Read ->
        { msg with
            Typ = ReadOk
            Messages = Some(state.Messages) }
        |> addInReply
        |> Some
    | Topology ->
        { msg with
            Typ = TopologyOk
            Topology = None }
        |> addInReply
        |> Some

let state = { Messages = [] }
Node.run (handler state)
