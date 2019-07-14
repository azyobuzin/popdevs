namespace PopDEVS

type internal ISimulationContext =
    abstract member GetTime : unit -> float
