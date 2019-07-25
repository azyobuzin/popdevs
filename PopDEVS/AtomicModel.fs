namespace PopDEVS

open System.Collections.Immutable

type AtomicModel<'I, 'O> internal (model) =
    inherit DevsModel<'I, 'O>(model)

module AtomicModel =
    [<CompiledName("Create")>]
    let create (transition: 'S * ElapsedTime * InputEventBuffer<'I> -> 'S,
                timeAdvance: 'S -> float,
                output: 'S -> 'O seq)
               (initialState: 'S) =
        let transition (s, e, i) =
            let inputBuf = InputEventBuffer(i)
            transition (unbox s, e, inputBuf) |> box

        let timeAdvance s =
            let ta = unbox s |> timeAdvance
            if ta < 0.0 then
                invalidOp "timeAdvance returned a negative number."
            ta

        let output s = unbox s |> output |> Seq.map box

        let model: BoxedAtomicModel =
            { Name = None;
              Transition = transition;
              TimeAdvance = timeAdvance;
              Output = output;
              InitialState = box initialState }
        AtomicModel<'I, 'O>(BoxedModel.Atomic model)

    [<CompiledName("WithName")>]
    let withName name (model: AtomicModel<'I, 'O>) =
        let inner =
            match model.Inner with
            | BoxedModel.Atomic x ->
                BoxedModel.Atomic { x with Name = Some name }
            | _ -> invalidArg "model" "model is not a valid AtomicModel"
        AtomicModel<'I, 'O>(inner)
