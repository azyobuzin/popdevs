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
        let endTime = (ProcessEnv.getTime env) + time
        WaitCondition<'I, unit>(TimeoutCondition(endTime))

    [<CompiledName("ReceiveEvent")>]
    static member receiveEvent (chooser: InputEventChooser<'I, 'R>) (_env: ProcessEnv<'I, 'O>) =
        WaitCondition<'I, 'R>(ReceiveEventCondition(chooser))
