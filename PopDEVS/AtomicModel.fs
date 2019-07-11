namespace PopDEVS

[<AbstractClass>]
type AtomicModel<'TInput, 'TOutput>() =
    inherit DevsModel<'TInput, 'TOutput>()

    abstract member InternalTransition : unit -> unit

    abstract member TimeAdvance : unit -> float

    abstract member Output : unit -> 'TOutput seq

    abstract member ExternalTransition : inputs: InputEventBag<'TInput> -> unit

    abstract member ConfluentTransition : inputs: InputEventBag<'TInput> -> unit

module AtomicModel =
    [<Sealed>]
    type private FSharpAtomicModel<'TState, 'TInput, 'TOutput>(internalTran, timeAdvance, output, externalTran, confluentTran, initialState) =
        inherit AtomicModel<'TInput, 'TOutput>()
        let mutable state: 'TState = initialState
    
        override __.InternalTransition() =
            state <- internalTran state
    
        override __.TimeAdvance() =
            timeAdvance state
    
        override __.Output() =
            output state
    
        override __.ExternalTransition(inputs) =
            state <- externalTran (state, inputs)
    
        override __.ConfluentTransition(inputs) =
            state <- confluentTran (state, inputs)
    
    let create (internalTran, timeAdvance, output, externalTran, confluentTran) initialState =
        FSharpAtomicModel<'TState, 'TInput, 'TOutput>(internalTran, timeAdvance, output, externalTran, confluentTran, initialState)
        :> AtomicModel<'TInput, 'TOutput>
