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

module Node =
    let jsonOptions = JsonConfig.create (serializeNone = SerializeNone.Omit)
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
        |> Json.serializeU

    let handleMessage<'T> (f: 'T -> 'T) json =
        let msg: Message<'T> = Json.deserialize json
        let newBody = f msg.Body

        { Src = msg.Dst
          Dst = msg.Src
          Body = newBody }
        |> Json.serializeU

    let rec run<'T> (f: 'T -> 'T) (initialized: bool) =
        match read () with
        | Some(ln) ->
            if initialized then handleMessage f ln else handleInit ln
            |> write

            run f true
        | None -> ()
