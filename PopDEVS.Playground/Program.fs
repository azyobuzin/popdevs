open PopDEVS
open PopDEVS.ProcessOriented
open PopDEVS.Sequential
open ProcessEnv

type ClaimerId = int

type PrinterManagerInput =
    | Request of ClaimerId
    | Release

type PrinterManagerOutput =
    | Done of ClaimerId
    | Report of int

type PrinterUserInput =
    | PrinterReady

let printerManager maxPrinters processEnv =
    processModel {
        let mutable units = maxPrinters
        while true do
            let! msg = processEnv |> receiveEvent (fun x ->
                match x.Event with
                | Request _ when units > 0 -> Some x.Event
                | Release -> Some x.Event
                | _ -> None)
            match msg with
            | Request claimer ->
                units <- units - 1
                emitOutput (Done claimer) processEnv
            | Release ->
                units <- units + 1
            emitOutput (Report units) processEnv
    }

let printerUser (requestInterval, utilizationTime, numTimesOfUse) claimerId processEnv =
    processModel {
        let mutable i = 0
        while i < numTimesOfUse do
            do! wait requestInterval processEnv

            // 利用申請
            emitOutput (Request claimerId) processEnv
            let! _ = receiveEvent anyEvent processEnv

            // 利用
            do! wait utilizationTime processEnv

            emitOutput Release processEnv
            i <- i + 1
    }

[<EntryPoint>]
let main argv =
    let coupledModelBuilder = CoupledModelBuilder()
    let managerModel =
        printerManager 2
        |> ProcessModel.createAtomicModel
        |> AtomicModel.withName "Manager"
    let managerModelRef = coupledModelBuilder.AddComponent(managerModel)

    let translateManagerToUser claimerId = function
        | Done x when x = claimerId -> Some PrinterReady
        | _ -> None

    let createUser arg claimerId =
        let userModel =
            printerUser arg claimerId
            |> ProcessModel.createAtomicModel
            |> AtomicModel.withName (sprintf "User %d" claimerId)
        let userModelRef = coupledModelBuilder.AddComponent(userModel)
        coupledModelBuilder.Connect(userModelRef, managerModelRef, Some)
        coupledModelBuilder.Connect(managerModelRef, userModelRef, translateManagerToUser claimerId)

    createUser (10.0, 10.0, 3) 1
    createUser (5.0, 10.0, 6) 2
    createUser (7.0, 15.0, 3) 3

    let printEvent (x: EventObserver.ObservedEvent<obj>) =
        printfn "%.1f %s: %O" x.Time (Option.defaultValue "(No Name)" x.Model.Name) x.Event
    EventObserver.observeAll printEvent coupledModelBuilder |> ignore

    let coupledModel = coupledModelBuilder.Build()
    let runner = SequentialRunner.Create(coupledModel)
    runner.RunUntil(infinity)

    0
