open FSharp.Json
open GGF.Lib

type GeneratorType =
    | [<JsonUnionCase("generate")>] Generate
    | [<JsonUnionCase("generate_ok")>] GenerateOk

type Message =
    { [<JsonField("id")>]
      Id: string option
      [<JsonField("type")>]
      Typ: GeneratorType
      [<JsonField("msg_id")>]
      MsgId: int option
      [<JsonField("in_reply_to")>]
      InReplyTo: int option }

type UniqueIDGenerator = { mutable id: int }

let handler (uig: UniqueIDGenerator) (node: string) (msg: Message) : Message =
    uig.id <- uig.id + 1

    match msg.Typ with
    | GenerateOk -> failwith "Invalid client message: generate_ok"
    | Generate ->
        { Id = Some($"{node}:%d{uig.id}")
          Typ = GenerateOk
          MsgId = msg.MsgId
          InReplyTo = msg.MsgId }

let uig = { id = 0 }
Node.run (handler uig) false
