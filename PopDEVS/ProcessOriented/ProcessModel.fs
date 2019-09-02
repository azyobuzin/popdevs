namespace PopDEVS.ProcessOriented

open PopDEVS

[<AutoOpen>]
module ProcessModelBuilder =
    let processModel<'I, 'O> = ProcessModelBuilderImpl.Builder<'I, 'O>()
    
module ProcessModel =
    let createAtomicModel<'I, 'O> (processModel: ProcessEnv<'I, 'O> -> ProcessModelBuilderImpl.BuilderResult<'I, 'O>) =
        raise (System.NotImplementedException()) :> AtomicModel<'I, 'O>
