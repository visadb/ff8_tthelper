open GameStateDetection

open DomainTypes
open System.Drawing

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    //Bootstrap.savePowerModifiersFromExampleScreenshots()
    //Bootstrap.saveCursorFromExampleScreenshot()
    //Bootstrap.saveDigitFilesFromExampleScreenshot()
    //Bootstrap.printDiffs()
    //Bootstrap.saveElementSymbolsFromExampleScreenshots()
    //Bootstrap.saveEmptyElementlessPlayGridSlotElementBitmaps()
    //Bootstrap.saveEmptyPlayGridSlotElementBitmaps()

    let screenshot = new Bitmap(screenshotDir + @"in-game\target_selection_0_0_without_elem.jpg")
    printfn "%O" <| readGameState screenshot
    screenshot.Dispose()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
