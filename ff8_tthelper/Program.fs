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

    //let initState = snd GameStateDetectionTest.screenshotGameStates.[0]
    //let childStates = AI.childStates initState

    //Array.append [|(-1,-1), initState|] childStates |> Array.iter (fun (move, state) ->
    //    printfn "%O-------------------------------------- (%d,%d) -> %d\n" state (fst move) (snd move) (AI.evaluateNode state))

    //let initState = snd GameStateDetectionTest.screenshotGameStates.[0]
    let mutable state = snd GameStateDetectionTest.targetSelectionGameStates.[0]

    while not (AI.isTerminalNode state) do
        printfn "%O-------------------------------------- %d\n" state (AI.evaluateNode state)
        let (move, value) = AI.getBestMove state 10
        printfn "Best move is %d -> (%d,%d) with value %d" (fst move) (snd move / 3) (snd move % 3) value
        state <- AI.executeMove state move

    printfn "%O-------------------------------------- %d\n" state (AI.evaluateNode state)

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
