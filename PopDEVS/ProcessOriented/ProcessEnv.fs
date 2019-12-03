namespace PopDEVS.ProcessOriented

open FSharpx.Collections
open PopDEVS

type ProcessEnv<'I, 'O> internal () =
    let mutable innerEnv : ISimEnv option = None
    let mutable outputBuffer = DList.empty

    member internal __.GetSimEnv() =
        match innerEnv with
        | Some x -> x
        | None -> invalidOp "ProcessEnv cannot be used out of simulation."

    member internal __.SetSimEnv(env) =
        innerEnv <- Some env

    member internal __.Reset() =
        innerEnv <- None
        let outputs = outputBuffer
        outputBuffer <- DList.empty
        outputs

    member internal this.AddOutput(ev: 'O) =
        this.GetSimEnv() |> ignore // Check state
        outputBuffer <- DList.conj ev outputBuffer

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

    // TODO: takeEventsInBuffer

    [<CompiledName("EmitOutput")>]
    let emitOutput ev (env: ProcessEnv<'I, 'O>) =
        env.AddOutput(ev)
