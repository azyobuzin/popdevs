namespace PopDEVS.ProcessOriented

open PopDEVS

type ProcessEnv<'I, 'O> internal (innerEnv: ISimEnv) =
    member private __.InnerEnv = innerEnv // SimEnv は transition で渡ってくるから、ここだと都合悪い

    [<CompiledName("GetTime")>]
    static member getTime (env: ProcessEnv<'I, 'O>) =
        env.InnerEnv.GetTime()

    [<CompiledName("RunIO")>]
    static member runIO action (env: ProcessEnv<'I, 'O>) =
        env.InnerEnv.RunIO(action)

    [<CompiledName("Wait")>]
    static member wait time (env: ProcessEnv<'I, 'O>) =
        let targetTime = (ProcessEnv.getTime env) + time
        let poll (_, env, _, _) =
            if SimEnv.getTime env >= targetTime then Some () else None
        WaitCondition<'I, _>(poll)

    [<CompiledName("ReceiveEvent")>]
    static member receiveEvent chooser (env: ProcessEnv<'I, 'O>) =
        let poll (_, _, _, inputBuf) =
            let events = inputBuf |> InputEventBuffer.takeWithLimit chooser 1
            match events.Length with
            | 0 -> None
            | 1 -> Some events.[0]
            | _ -> failwith "takeWithLimit returns more than 1 events."
        WaitCondition<'I, _>(poll)
