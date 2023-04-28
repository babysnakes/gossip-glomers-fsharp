namespace GGF.Lib

open System
open FSharp.Json

type Message<'T> =
    { [<JsonField("src")>]
      Src: string
      [<JsonField("dest")>]
      Dst: string
      [<JsonField("body")>]
      Body: 'T }

type InitValue =
    | [<JsonUnionCase("init")>] Init
    | [<JsonUnionCase("init_ok")>] InitOk

type InitRequest =
    { [<JsonField("type")>]
      Typ: InitValue
      [<JsonField("msg_id")>]
      MsgId: int option
      [<JsonField("node_id")>]
      NodeId: string
      [<JsonField("node_ids")>]
      NodeIds: string list }

type InitResponse =
    { [<JsonField("type")>]
      Typ: InitValue
      [<JsonField("in_reply_to")>]
      InReplyTo: int option }

type Node =
    { Name: string
      Neighbours: string list }

type MessageWithSource<'Msg> = MessageWithSource of string * 'Msg
type Dispatcher<'Msg> = string -> 'Msg -> unit
type MessageHandler<'Msg, 'State> = 'State -> Node -> Dispatcher<'Msg> -> MessageWithSource<'Msg> -> 'State

module Node =
    // Generate unformatted json without None values.
    let jsonOptions =
        JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let private send (ln: string) = Console.WriteLine(ln)
    let private receive () = Console.ReadLine()

    let sendMessage<'Msg> (src: string) (dest: string) (msg: 'Msg) =
        { Src = src; Dst = dest; Body = msg } |> Json.serializeEx jsonOptions |> send

    let private initialize () : Node =
        let msg: Message<InitRequest> = receive () |> Json.deserialize
        sendMessage msg.Dst msg.Src { Typ = InitOk; InReplyTo = msg.Body.MsgId }

        msg.Body.NodeIds
        |> Set.ofList
        |> Set.remove msg.Body.NodeId
        |> Set.toList
        |> fun neighbours ->
            { Name = msg.Body.NodeId
              Neighbours = neighbours }

    let private handleMessageLoop<'Msg, 'State> (state: 'State) (f: MessageHandler<'Msg, 'State>) (node: Node) =
        let dispatcher = sendMessage<'Msg> node.Name

        let rec loop (f: MessageHandler<'Msg, 'State>) (state: 'State) =
            let msg: Message<'Msg> = receive () |> Json.deserialize
            f state node dispatcher (MessageWithSource(msg.Src, msg.Body)) |> loop f

        loop f state

    let run<'Msg, 'State> (state: 'State) (f: MessageHandler<'Msg, 'State>) =
        initialize () |> handleMessageLoop state f |> ignore
