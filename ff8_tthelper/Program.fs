open GameStateDetection

open System.Drawing

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()


    //Bootstrap.saveDigitFilesFromExampleScreenshot()
    //printDiffs()

    let screenshot = new Bitmap(imageDir + "example_screenshot.jpg")
    printfn "Strengths: %A" <| readCardStrengths screenshot myCards.[1]
    screenshot.Dispose()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0