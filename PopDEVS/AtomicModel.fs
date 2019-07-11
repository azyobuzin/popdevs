namespace PopDEVS

open System.Collections.Immutable

type ElapsedTime =
    { Completed: bool; Elapsed: float }

[<AbstractClass>]
type AtomicModel<'TInput, 'TOutput>() =
    inherit DevsModel<'TInput, 'TOutput>()

    abstract member Transition : elapsed: ElapsedTime * inputs: InputEventBag<'TInput> -> unit

    abstract member TimeAdvance : unit -> float

    abstract member Output : unit -> 'TOutput seq

module AtomicModel =
    [<Sealed>]
    type private FSharpAtomicModel<'TState, 'TInput, 'TOutput>(transition, timeAdvance, output, initialState) =
        inherit AtomicModel<'TInput, 'TOutput>()

        let mutable state: 'TState = initialState
    
        override __.Transition(elapsed, inputs) =
            state <- transition (state, elapsed, inputs)
            
        override __.TimeAdvance() =
            timeAdvance state
    
        override __.Output() =
            output state
    
    let create (transition, timeAdvance, output) initialState =
        FSharpAtomicModel<'TState, 'TInput, 'TOutput>(transition, timeAdvance, output, initialState)
        :> AtomicModel<'TInput, 'TOutput>

    type internal BoxedAtomicModel =
        { Transition: ElapsedTime * obj seq -> unit;
          TimeAdvance: unit -> float;
          Output: unit -> obj seq }

    let internal createProxy (model: AtomicModel<_, _>) =
        {
            Transition = (fun (elapsed, inputs) ->
                let inputs = InputEventBag(ImmutableArray.CreateRange(Seq.map unbox inputs))
                model.Transition(elapsed, inputs));
            TimeAdvance = (fun _ -> model.TimeAdvance());
            Output = (fun _ -> model.Output() |> Seq.map box);
        }
