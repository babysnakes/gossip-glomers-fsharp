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

let handler (id: uint) (MessageWithSource(_: string, msg: Message)) (node: Node) =
    match msg.Typ with
    | GenerateOk -> failwith "Invalid client message: generate_ok"
    | Generate ->
        { Id = Some($"{node}:%d{id}")
          Typ = GenerateOk
          MsgId = msg.MsgId
          InReplyTo = msg.MsgId }
        |> Some
        |> fun response -> (id + 1u, response)

Node.run 1u handler
