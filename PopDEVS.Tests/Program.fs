open Expecto

[<EntryPoint>]
let main argv =
    let writeResults = TestResults.writeNUnitSummary ("TestResults.xml", "PopDEVS.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsInAssembly config argv
