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

type MessageHandler<'T> = Node -> 'T -> 'T

module Node =
    // Generate unformatted json without None values.
    let jsonOptions =
        JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let send (ln: string) = Console.WriteLine(ln)
    let private receive () = Console.ReadLine()

    let private initialize: Node =
        let msg: Message<InitRequest> = receive () |> Json.deserialize

        { Src = msg.Dst
          Dst = msg.Src
          Body =
            { Typ = InitOk
              InReplyTo = msg.Body.MsgId } }
        |> Json.serializeEx jsonOptions
        |> send

        let neighbours =
            msg.Body.NodeIds |> Set.ofList |> Set.remove msg.Body.NodeId |> Set.toList

        { Name = msg.Body.NodeId
          Neighbours = neighbours }

    let rec private handleMessageLoop<'T> (f: MessageHandler<'T>) (node: Node) =
        let msg: Message<'T> = receive () |> Json.deserialize
        let newBody = f node msg.Body

        { Src = msg.Dst
          Dst = msg.Src
          Body = newBody }
        |> Json.serializeEx jsonOptions
        |> send

        handleMessageLoop f node

    let run<'T> (f: MessageHandler<'T>) =
        initialize |> handleMessageLoop f |> ignore
