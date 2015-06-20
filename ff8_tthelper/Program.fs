open GameStateDetection

open DomainTypes
open System.Drawing

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    //Bootstrap.saveCursorFromExampleScreenshot()
    //Bootstrap.saveSelectionCursorsFromExampleScreenshot()
    //printDiffs()

    let screenshot = new Bitmap(imageDir + "example_screenshot_2.jpg")
    printfn "%O" <| readGameState screenshot
    screenshot.Dispose()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0