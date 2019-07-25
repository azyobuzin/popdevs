namespace PopDEVS.Sequential

open PopDEVS

type SequentialRunner internal (model: DevsModel, initialTime: float) =
    let rootCoordinator = Coordinator.create model
    do rootCoordinator.Initialize(initialTime)

    static member Create(model: DevsModel<VoidEvent, _>, initialTime: float) =
        SequentialRunner(model, initialTime)

    static member Create(model: DevsModel<VoidEvent, _>) =
        SequentialRunner(model, 0.0)

    member __.Time = rootCoordinator.NextTime

    member this.Step() =
        rootCoordinator.AdvanceSimulation(this.Time)

    member this.RunUntil(time: float) =
        while this.Time < time do
            rootCoordinator.AdvanceSimulation(this.Time)
