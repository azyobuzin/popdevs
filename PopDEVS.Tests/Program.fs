module PopDEVS.Tests.Program

open Expecto

[<EntryPoint>]
let main argv =
    let mutable config = defaultConfig

    let argv =
        if argv |> Array.contains "--nunit" then
            let writeResults = TestResults.writeNUnitSummary ("TestResults.xml", "PopDEVS.Tests")
            config <- config.appendSummaryHandler writeResults
            argv |> Array.except ["--nunit"]
        else
            argv

    runTestsInAssembly config argv
