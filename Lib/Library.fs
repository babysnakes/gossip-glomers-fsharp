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

type MessageHandler<'T> = string -> 'T -> 'T

module Node =
    // Generate unformatted json without None values.
    let jsonOptions =
        JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let private write (ln: string) = Console.WriteLine(ln)

    let private read () =
        let input = Console.ReadLine()

        if String.IsNullOrEmpty(input) then None else Some(input)

    let handleInit json =
        let msg: Message<InitRequest> = Json.deserialize json

        { Src = msg.Dst
          Dst = msg.Src
          Body =
            { Typ = InitOk
              InReplyTo = msg.Body.MsgId } }
        |> Json.serializeEx jsonOptions

    let handleMessage<'T> (f: MessageHandler<'T>) json =
        let msg: Message<'T> = Json.deserialize json
        let handler = f msg.Dst
        let newBody = handler msg.Body

        { Src = msg.Dst
          Dst = msg.Src
          Body = newBody }
        |> Json.serializeEx jsonOptions

    let rec run<'T> (f: MessageHandler<'T>) (initialized: bool) =
        match read () with
        | Some(ln) ->
            if initialized then handleMessage f ln else handleInit ln
            |> write

            run f true
        | None -> ()
