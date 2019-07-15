namespace PopDEVS

open System
open System.Collections.Immutable

type AtomicModel<'I, 'O> internal (model) =
    inherit DevsModel<'I, 'O>(model)

module AtomicModel =
    let create (transition: 'S * ElapsedTime * InputEventBag<'I> -> 'S,
                timeAdvance: 'S -> float,
                output: 'S -> 'O seq)
               (initialState: 'S) =
        let transition (s, e, i) =
            let inputBag = InputEventBag(ImmutableArray.CreateRange(Seq.map unbox i))
            transition (unbox s, e, inputBag) |> box

        let timeAdvance s =
            let ta = unbox s |> timeAdvance
            if ta < 0.0 then
                raise (InvalidOperationException("timeAdvance returned a negative number."))
            ta

        let output s = unbox s |> output |> Seq.map box

        let model: BoxedDevsModel.AtomicModel =
            { Name = None;
              Transition = transition;
              TimeAdvance = timeAdvance;
              Output = output;
              InitialState = box initialState }
        AtomicModel<'I, 'O>(BoxedDevsModel.Model.Atomic model)

    let withName name (model: AtomicModel<'I, 'O>) =
        let inner =
            match model.Inner with
            | BoxedDevsModel.Model.Atomic x ->
                BoxedDevsModel.Model.Atomic { x with Name = Some name }
            | _ -> raise (ArgumentException("model is not a valid AtomicModel"))
        AtomicModel<'I, 'O>(inner)
