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
type MessageHandler<'Msg, 'State> = 'State -> MessageWithSource<'Msg> -> Node -> Tuple<'State, 'Msg option>

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

    let rec private handleMessageLoop<'Msg, 'State> (state: 'State) (f: MessageHandler<'Msg, 'State>) (node: Node) =
        let msg: Message<'Msg> = receive () |> Json.deserialize
        let newState, response = f state (MessageWithSource(msg.Src, msg.Body)) node

        match response with
        | Some(newBody) ->
            { Src = msg.Dst
              Dst = msg.Src
              Body = newBody }
            |> Json.serializeEx jsonOptions
            |> send
        | None -> ()

        handleMessageLoop newState f node

    let run<'Msg, 'State> (state: 'State) (f: MessageHandler<'Msg, 'State>) =
        initialize |> handleMessageLoop state f |> ignore
