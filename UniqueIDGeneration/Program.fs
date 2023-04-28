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

let handler (id: uint) _ (dispatch: Dispatcher<Message>) (MessageWithSource(src, msg)) =
    match msg.Typ with
    | GenerateOk -> failwith "Invalid client message: generate_ok"
    | Generate ->
        { Id = Some($"{src}:%d{id}")
          Typ = GenerateOk
          MsgId = msg.MsgId
          InReplyTo = msg.MsgId }
        |> dispatch src
    
    id + 1u

Node.run 1u handler
