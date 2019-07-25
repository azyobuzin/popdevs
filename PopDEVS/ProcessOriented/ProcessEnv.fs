namespace PopDEVS.ProcessOriented

open PopDEVS

type ProcessEnv<'I, 'O> internal (innerEnv: ISimEnv) =
    member private __.InnerEnv = innerEnv

    [<CompiledName("GetTime")>]
    static member getTime (env: ProcessEnv<'I, 'O>) =
        env.InnerEnv.GetTime()

    [<CompiledName("RunIO")>]
    static member runIO action (env: ProcessEnv<'I, 'O>) =
        env.InnerEnv.RunIO(action)
