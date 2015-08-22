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

    //let screenshot = new Bitmap(screenshotDir + @"in-game\target_selection_0_0_without_elem.jpg")
    //printfn "%O" <| readGameState screenshot
    //screenshot.Dispose()

    let initState = snd GameStateDetectionTest.screenshotGameStates.[0]
    let childStates = AI.childStates initState

    Array.append [|initState|] childStates |> Array.iter (fun state ->
        printfn "%O-------------------------------------- %d\n" state (AI.evaluateNode state))

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
