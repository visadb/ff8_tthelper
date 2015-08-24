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
    let printState state =
        printfn "%O-------------------------------------- v=%d b=%d\n" state (AI.evaluateNode state false) (AI.cardBalance state)


    let mutable state = initState
    let sw = new System.Diagnostics.Stopwatch()
    while not (AI.isTerminalNode state) do
        printState state
        sw.Restart()
        let (move, value) = AI.getBestMove state 9
        printfn "Best move is %d -> (%d,%d) with value %d (took %d ms)" (fst move)
                                                                        (snd move / 3) (snd move % 3)
                                                                        value
                                                                        sw.ElapsedMilliseconds
        state <- AI.executeMove state move

    printState state

let readGameStateFromScreenshot (screenshotPath: string) =
    let sw = new System.Diagnostics.Stopwatch()
    sw.Restart()
    printfn "Reading %s" screenshotPath
    let screenshot = SimpleBitmap.fromFile(screenshotPath)
    printfn "Bitmap of size %dx%d read in %d ms" screenshot.Width screenshot.Height sw.ElapsedMilliseconds
    sw.Restart()
    let state = readGameState screenshot
    printfn "GameState read in %d ms" sw.ElapsedMilliseconds
    state

let watchScreenshotDir () =
    let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
    while true do
        printfn "Waiting for new screenshot..."
        let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
        printfn "#######################################"
        printfn "#### DETECTED %s" changedResult.Name
        printfn "#######################################"
        playGame <| readGameStateFromScreenshot(steamScreenshotDir + @"\" + changedResult.Name)

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

    watchScreenshotDir()
    //playScreenshot <| screenshotDir + @"in-game\example_screenshot_4.jpg"
    //playScreenshot <| steamScreenshotDir + @"\2015-08-16_00001.jpg"
    //playTestState()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
