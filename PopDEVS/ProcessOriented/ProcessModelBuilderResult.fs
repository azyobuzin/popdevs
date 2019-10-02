namespace PopDEVS.ProcessOriented

type ProcessModelBuilderResult<'I> internal (cfg: ControlFlowGraph.Graph) =
    member _.ControlFlowGraph = cfg
