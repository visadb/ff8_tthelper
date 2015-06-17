open GameStateDetection

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    saveDigitFilesFromExampleScreenshot()
    printDiffs()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0