namespace PopDEVS.ProcessOriented

open System.Collections.Generic
open PopDEVS

type ProcessEnv<'I, 'O> internal () =
    let mutable innerEnv : ISimEnv option = None
    let outputBuffer = List<'O>()

    member internal __.GetSimEnv() =
        match innerEnv with
        | Some x -> x
        | None -> invalidOp "ProcessEnv cannot be used out of simulation."

    member internal __.SetSimEnv(env) =
        innerEnv <- Some env

    member internal __.Reset() =
        innerEnv <- None
        let outputs = List.ofSeq outputBuffer
        outputBuffer.Clear()
        outputs

    [<CompiledName("GetTime")>]
    static member getTime (env: ProcessEnv<'I, 'O>) =
        env.GetSimEnv().GetTime()

    [<CompiledName("RunIO")>]
    static member runIO action (env: ProcessEnv<'I, 'O>) =
        env.GetSimEnv().RunIO(action)

    [<CompiledName("Wait")>]
    static member wait time (env: ProcessEnv<'I, 'O>) =
        let endTime = (ProcessEnv.getTime env) + time
        WaitCondition<'I, unit>(TimeoutCondition(endTime))

    [<CompiledName("ReceiveEvent")>]
    static member receiveEvent (chooser: InputEventChooser<'I, 'R>) (_env: ProcessEnv<'I, 'O>) =
        WaitCondition<'I, 'R>(ReceiveEventCondition(chooser))

    // TODO: 出力を登録
