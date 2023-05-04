namespace GGF.Lib

open System
open FSharp.Json

module Logger =
    type private LogMessage =
        | Log of string
        | Enable of bool

    let private agent =
        MailboxProcessor<LogMessage>.Start(fun inbox ->
            let rec loop enabled =
                async {
                    let! msg = inbox.Receive()

                    let newEnabled =
                        match msg with
                        | Log msg ->
                            if enabled then eprintfn $"{msg}"
                            enabled
                        | Enable enable -> enable

                    return! loop newEnabled
                }

            loop false)

    /// Log a message
    let log (msg: string) = agent.Post(Log(msg))

    /// log message with "RECV: " prefix and return original message for composing
    let logReceived (msg: string) =
        log $"RECV: {msg}"
        msg

    /// log message with "SENT: " prefix and return original message for composing
    let logSent (msg: string) =
        log $"SENT: {msg}"
        msg

    /// Enable log messages
    let enableLogger () = agent.Post(Enable(true))
    /// Disable log messages
    let disableLogger () = agent.Post(Enable(false))

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

type MessageData<'Msg> = MessageData of string * 'Msg
type Dispatcher<'Msg> = string -> 'Msg -> unit
type MessageHandler<'Msg, 'State> = 'State -> Node -> Dispatcher<'Msg> -> MessageData<'Msg> -> 'State
type BackgroundAgent<'Msg> = Dispatcher<'Msg> -> unit

type AgentData<'Msg> =
    { Agent: BackgroundAgent<'Msg>
      Frequency: int }

type HandlerData<'Msg, 'State> =
    { Handler: MessageHandler<'Msg, 'State>
      State: 'State }

module Node =
    // Generate unformatted json without None values.
    let jsonOptions =
        JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let private send (ln: string) =
        Logger.logSent ln |> ignore
        Console.WriteLine(ln)
    let private receive () = Console.ReadLine()

    let sendMessage<'Msg> (src: string) (dest: string) (msg: 'Msg) =
        { Src = src; Dst = dest; Body = msg } |> Json.serializeEx jsonOptions |> send

    let private mkLoop (dispatch: Dispatcher<'Msg>) (agentData: AgentData<'Msg>) =
        let rec loop () =
            async {
                do! Async.Sleep agentData.Frequency
                do agentData.Agent dispatch
                return! loop ()
            }

        loop ()

    let private initialize<'Msg> (agentData: AgentData<'Msg> option) : Node =
        let msg: Message<InitRequest> = receive () |> Logger.logReceived |> Json.deserialize
        let dispatcher = sendMessage msg.Dst

        { Typ = InitOk
          InReplyTo = msg.Body.MsgId }
        |> sendMessage msg.Dst msg.Src

        match agentData with
        | Some ad -> mkLoop dispatcher ad |> Async.Start
        | None -> ()

        msg.Body.NodeIds
        |> Set.ofList
        |> Set.remove msg.Body.NodeId
        |> Set.toList
        |> fun neighbours ->
            { Name = msg.Body.NodeId
              Neighbours = neighbours }

    let private handleMessages<'Msg, 'State> (hData: HandlerData<'Msg, 'State>) (node: Node) =
        let dispatcher = sendMessage<'Msg> node.Name

        let rec loop (f: MessageHandler<'Msg, 'State>) (state: 'State) =
            let msg: Message<'Msg> = receive () |> Logger.logReceived |> Json.deserialize
            f state node dispatcher (MessageData(msg.Src, msg.Body)) |> loop f

        loop hData.Handler hData.State

    let run<'Msg, 'State> (ad: AgentData<'Msg> option) (hData: HandlerData<'Msg, 'State>) =
        initialize ad |> handleMessages hData |> ignore
