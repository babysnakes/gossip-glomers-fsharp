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

type State = { mutable Messages: int list }

let addInReply (msg: Message) : Message = { msg with InReplyTo = msg.MsgId }

let handler (state: State) (_: Node) (msg: Message) : Message =
    match msg.Typ with
    | BroadcastOk
    | ReadOk
    | TopologyOk -> failwith "node received OK type"
    | Broadcast ->
        state.Messages <- msg.Message.Value :: state.Messages

        { msg with
            Typ = BroadcastOk
            Message = None }
        |> addInReply
    | Read ->
        { msg with
            Typ = ReadOk
            Messages = Some(state.Messages) }
        |> addInReply
    | Topology ->
        { msg with
            Typ = TopologyOk
            Topology = None }
        |> addInReply

let state = { Messages = [] }
Node.run (handler state)
