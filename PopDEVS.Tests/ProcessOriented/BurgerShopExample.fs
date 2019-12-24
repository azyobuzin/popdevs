module PopDEVS.Tests.ProcessOriented.BurgerShopExample

open System.Collections.Generic
open Expecto
open FSharpx.Collections
open PopDEVS
open PopDEVS.ProcessOriented
open PopDEVS.Sequential
open ProcessEnv

type CustomerId = int
type OrderDifficulty = float
type CustomerInput = Called | ReceiveGoods
type CustomerOutput = LineUp | Order of OrderDifficulty
let customer (cid: CustomerId, visitTime, orderDifficulty) =
    (fun processEnv ->
        processModel {
            // 来店時刻になるまで待機
            do! wait visitTime processEnv
            // 行列に並ぶ
            emitOutput LineUp processEnv
            // 呼ばれるまで待つ
            do! receiveEvent (eventIfUnit ((=) Called)) processEnv
            // 注文をする
            emitOutput (Order orderDifficulty) processEnv
            // 商品を受け取る
            do! receiveEvent (eventIfUnit ((=) ReceiveGoods)) processEnv
        })
    |> ProcessModel.createAtomicModel
    |> AtomicModel.withName (sprintf "Customer %d" cid)

type QueueInput = Enqueue of CustomerId | Dequeue
type QueueOutput = Dequeued of CustomerId
let queue =
    let transition ((queue, dequeued), _, elapsed, inputBuf) =
        // Enqueue イベントを処理
        let queue =
            inputBuf
            |> InputEventBuffer.take (function Enqueue cid -> Some cid | _ -> None)
            |> Seq.fold (fun q cid -> Queue.conj cid q) queue

        // Dequeue イベントを処理
        let dequeued = if elapsed.Completed then DList.empty else dequeued
        inputBuf |> InputEventBuffer.takeWithLimit (eventIfUnit ((=) Dequeue)) queue.Length
            |> Seq.fold (fun (q, d) () -> let x, q = Queue.uncons q in q, DList.conj x d) (queue, dequeued)

    let timeAdvance (_, d) = if DList.isEmpty d then infinity else 0.0
    let output (_, d) = d |> DList.toSeq |> Seq.map Dequeued

    AtomicModel.create (transition, timeAdvance, output) (Queue.empty, DList.empty)
    |> AtomicModel.withName "Queue"

type ClerkInput = NextCustomer of CustomerId | ReceiveOrder of OrderDifficulty
type ClerkOutput = WaitCustomer | CallCustomer of CustomerId | PassGoods of CustomerId
let clerk =
    (fun processEnv ->
        processModel {
            while true do
                // 次の客が来るまで待つ
                emitOutput WaitCustomer processEnv
                let! cid =
                    processEnv |> receiveEvent
                        (function NextCustomer cid -> Some cid | _ -> None)
                // 行列の先頭の人を呼び出す
                emitOutput (CallCustomer cid) processEnv
                // 注文を待つ
                let! difficulty =
                    processEnv |> receiveEvent
                        (function ReceiveOrder d -> Some d | _ -> None)
                // 注文を処理する
                do! wait difficulty processEnv
                // 商品を渡す
                emitOutput (PassGoods cid) processEnv
        })
    |> ProcessModel.createAtomicModel
    |> AtomicModel.withName "Clerk"

let private observe (builder: CoupledModelBuilder<_, _>, cref, dst: List<_>) =
    EventObserver.observe
        (fun x -> dst.Add((x.Time, x.Event)))
        builder cref
    |> ignore

let burgerShopExample () =
    let systemBuilder = CoupledModelBuilder<VoidEvent, VoidEvent>()
    
    let queueRef = systemBuilder.AddComponent(queue)
    let queueEvents = List()
    observe (systemBuilder, queueRef, queueEvents)

    let clerkRef = systemBuilder.AddComponent(clerk)
    let clerkEvents = List()
    observe (systemBuilder, clerkRef, clerkEvents)

    // Queue と Clerk の関係を定義
    systemBuilder.Connect(clerkRef, queueRef, (function WaitCustomer -> Some Dequeue | _ -> None))
    systemBuilder.Connect(queueRef, clerkRef, (function Dequeued cid -> Some (NextCustomer cid)))

    // Customer を作成
    let createCustomer ((cid, _, _) as param) =
        let model = customer param
        let customerRef = systemBuilder.AddComponent(model)

        // Customer と Queue の関係を定義
        systemBuilder.Connect(customerRef, queueRef, (function
            | LineUp -> Some (Enqueue cid) | _ -> None))
        // Customer と Clerk の関係を定義
        systemBuilder.Connect(customerRef, clerkRef, (function
            | Order d -> Some (ReceiveOrder d) | _ -> None))
        systemBuilder.Connect(clerkRef, customerRef, (function
            | CallCustomer x when x = cid -> Some Called
            | PassGoods x when x = cid -> Some ReceiveGoods
            | _ -> None))

        let events = List()
        observe (systemBuilder, customerRef, events)

        customerRef, events
    let _customer1Ref, customer1Events = createCustomer (1, 1.0, 5.0)
    let _customer2Ref, customer2Events = createCustomer (2, 2.0, 3.0)
    let _customer3Ref, customer3Events = createCustomer (3, 12.0, 8.0)

    let system = systemBuilder.Build()
    let runner = SequentialRunner.Create(system)
    runner.RunUntil(infinity)

    // 結果検証
    Expect.sequenceEqual queueEvents
        [1.0, Dequeued 1; 6.0, Dequeued 2; 12.0, Dequeued 3]
        "Queue"
    Expect.sequenceEqual clerkEvents
        [
            0.0, WaitCustomer
            1.0, CallCustomer 1; 6.0, PassGoods 1
            6.0, WaitCustomer
            6.0, CallCustomer 2; 9.0, PassGoods 2
            9.0, WaitCustomer
            12.0, CallCustomer 3; 20.0, PassGoods 3
            20.0, WaitCustomer
        ]
        "Clerk"
    Expect.sequenceEqual customer1Events
        [1.0, LineUp; 1.0, Order 5.0]
        "Customer 1"
    Expect.sequenceEqual customer2Events
        [2.0, LineUp; 6.0, Order 3.0]
        "Customer 2"
    Expect.sequenceEqual customer3Events
        [12.0, LineUp; 12.0, Order 8.0]
        "Customer 3"

[<Tests>]
let tests =
    testList "ProcessModel" [
        testCase "burger shop example" burgerShopExample
    ]
