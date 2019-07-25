namespace PopDEVS

type ISimEnv =
    abstract member GetTime : unit -> float
    abstract member RunIO : action: (unit -> 'a) -> 'a

module SimEnv =
    /// <summary>シミュレーションの時刻を返します。</summary>
    [<CompiledName("GetTime")>]
    let getTime (env: ISimEnv) =
        env.GetTime()

    /// <summary>I/O 操作を行います。指定した関数は、一度だけ実行されます。</summary>
    /// <remarks>楽観的同期を使用するとき、安全に I/O を実行できる時刻になるまで、この操作を保留します。</remarks>
    [<CompiledName("RunIO")>]
    let runIO action (env: ISimEnv) =
        env.RunIO(action)
