open GameStateDetection

open DomainTypes
open System.Drawing

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()


    //Bootstrap.saveDigitFilesFromExampleScreenshot()
    //printDiffs()

    let screenshot = new Bitmap(imageDir + "example_screenshot.jpg")
    printfn "%s" (handToString <| readHand screenshot myHandCardPositions (Some 0))
    screenshot.Dispose()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0