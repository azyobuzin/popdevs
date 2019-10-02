namespace PopDEVS.ProcessOriented

open PopDEVS

[<AutoOpen>]
module ProcessModelBuilder =
    let processModel<'I> = ProcessModelBuilderImpl.Builder<'I>()
    
module ProcessModel =
    let createAtomicModel<'I, 'O> (processModel: ProcessEnv<'I, 'O> -> ProcessModelBuilderResult<'I>) =
        raise (System.NotImplementedException()) :> AtomicModel<'I, 'O>
