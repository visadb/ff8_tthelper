open GameStateDetection

open DomainTypes
open System.Drawing
open GameStateDetectionTest

let steamScreenshotDir = @"D:\Program Files\Steam\userdata\33243684\760\remote\39150\screenshots"

let bootstrap () =
    //Bootstrap.savePowerModifiersFromExampleScreenshots()
    //Bootstrap.saveCursorFromExampleScreenshot()
    //Bootstrap.saveDigitFilesFromExampleScreenshot()
    //Bootstrap.printDiffs()
    //Bootstrap.saveElementSymbolsFromExampleScreenshots()
    //Bootstrap.saveEmptyElementlessPlayGridSlotElementBitmaps()
    //Bootstrap.saveEmptyPlayGridSlotElementBitmaps()
    ()

let playGame initState =
    let mutable state = initState
    let sw = new System.Diagnostics.Stopwatch()
    while not (AI.isTerminalNode state) do
        printfn "%O-------------------------------------- %d" state (AI.evaluateNode state)
        sw.Restart()
        let (move, value) = AI.getBestMove state 9
        printfn "Best move is %d -> (%d,%d) with value %d (took %d ms)" (fst move)
                                                                        (snd move / 3) (snd move % 3)
                                                                        value
                                                                        sw.ElapsedMilliseconds
        state <- AI.executeMove state move

    printfn "%O-------------------------------------- %d\n" state (AI.evaluateNode state)

let readGameStateFromScreenshot (screenshotPath: string) =
    let screenshot = new Bitmap(screenshotPath)
    let state = readGameState screenshot
    screenshot.Dispose()
    state

let watchScreenshotDir () =
    let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
    let sw = new System.Diagnostics.Stopwatch()
    while true do
        printfn "Waiting for new screenshot..."
        let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
        printfn "#######################################"
        printfn "#### DETECTED %s" changedResult.Name
        printfn "#######################################"
        sw.Restart()

        let state = readGameStateFromScreenshot(steamScreenshotDir + @"\" + changedResult.Name)
        printfn "GameState read in %d ms" sw.ElapsedMilliseconds

        playGame state

    watcher.Dispose()

let playScreenshot (screenshotPath: string) =
    playGame <| readGameStateFromScreenshot screenshotPath
    
let playTestState () =
    let state = {
            turnPhase = OpponentsTurn
            myHand = [|None; None; None; None             ; hc [1;2;1;1] Me n|]
            opHand = [|None; None; None; hc [1;1;1;1] Op n; hc [1;1;2;1] Op n|]
            playGrid = { slots = [| pc [1;1;1;1] Me 0; pc [1;1;1;1] Op 0; pc [1;1;1;1] Me 0
                                    pc [1;1;2;1] Op 0; emptySlot        ; pc [1;2;1;1] Me 0
                                    pc [1;1;1;1] Op 0; emptySlot        ; pc [1;1;1;1] Me 0 |] } }
    playGame state

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    //watchScreenshotDir()
    //playScreenshot <| screenshotDir + @"in-game\example_screenshot_4.jpg"
    //playScreenshot <| steamScreenshotDir + @"\2015-08-16_00001.jpg"
    playTestState()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
