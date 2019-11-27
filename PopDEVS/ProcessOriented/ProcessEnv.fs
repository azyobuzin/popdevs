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

    member internal this.AddOutput(ev) =
        this.GetSimEnv() |> ignore // Check state
        outputBuffer.Add(ev)

module ProcessEnv =
    [<CompiledName("GetTime")>]
    let getTime (env: ProcessEnv<'I, 'O>) =
        env.GetSimEnv().GetTime()

    [<CompiledName("RunIO")>]
    let runIO action (env: ProcessEnv<'I, 'O>) =
        env.GetSimEnv().RunIO(action)

    [<CompiledName("Wait")>]
    let wait time (env: ProcessEnv<'I, 'O>) =
        let endTime = (getTime env) + time
        WaitCondition<'I, unit>(TimeoutCondition(endTime))

    [<CompiledName("ReceiveEvent")>]
    let receiveEvent (chooser: InputEventChooser<'I, 'R>) (_env: ProcessEnv<'I, 'O>) =
        WaitCondition<'I, 'R>(ReceiveEventCondition(chooser))

    [<CompiledName("EmitOutput")>]
    let emitOutput ev (env: ProcessEnv<'I, 'O>) =
        env.AddOutput(ev)
