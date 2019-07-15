namespace PopDEVS

type SequentialRunner internal (model: DevsModel, initialTime: float) =
    let rootCoordinator = SequentialCoordinator.create model
    do rootCoordinator.Initialize(initialTime)

    static member Create(model: DevsModel<NoInput, _>, initialTime: float) =
        SequentialRunner(model, initialTime)

    static member Create(model: DevsModel<NoInput, _>) =
        SequentialRunner(model, 0.0)

    member __.Time = rootCoordinator.NextTime

    member this.Step() =
        Simulation.setContext this
        rootCoordinator.AdvanceSimulation(this.Time)

    member this.RunUntil(time: float) =
        Simulation.setContext this
        while this.Time < time do
            rootCoordinator.AdvanceSimulation(this.Time)

    interface ISimulationContext with
        member this.GetTime() = this.Time
