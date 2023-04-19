﻿open GGF.Lib
open FSharp.Json

type EchoTyp =
    | [<JsonUnionCase("echo")>] Echo
    | [<JsonUnionCase("echo_ok")>] EchoOk

type Message =
    { [<JsonField("type")>]
      Typ: EchoTyp
      [<JsonField("msg_id")>]
      MsgId: int option
      [<JsonField("in_reply_to")>]
      InReplyTo: int option
      [<JsonField("echo")>]
      Echo: string }

let handle (_: Node) (msg: Message) : Message option =
    { msg with
        InReplyTo = msg.MsgId
        Typ = EchoOk }
    |> Some

Node.run handle
