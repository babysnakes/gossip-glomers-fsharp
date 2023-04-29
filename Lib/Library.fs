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

    let private send (ln: string) = Console.WriteLine(ln)
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
        let msg: Message<InitRequest> = receive () |> Json.deserialize
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
            let msg: Message<'Msg> = receive () |> Json.deserialize
            f state node dispatcher (MessageData(msg.Src, msg.Body)) |> loop f

        loop hData.Handler hData.State

    let run<'Msg, 'State> (ad: AgentData<'Msg> option) (hData: HandlerData<'Msg, 'State>) =
        initialize ad |> handleMessages hData |> ignore
