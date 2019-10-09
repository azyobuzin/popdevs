open PopDEVS.ProcessOriented

[<EntryPoint>]
let main argv =
    let x =
        let env = Unchecked.defaultof<ProcessEnv<int, unit>>
        processModel<int> {
            let mutable i = 1
            while i <= 9 do
                let! v = ProcessEnv.receiveEvent (fun x -> Some x.Event) env
                if v % 2 = 0 then
                    do! ProcessEnv.wait 10.0 env
                i <- i + 1
        }

    printfn "%O" x.ControlFlowGraph
    0
