open GameStateDetection

open DomainTypes
open System.Drawing

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

let watchScreenshotDir () =
    let steamScreenshotDir = @"D:\Program Files\Steam\userdata\33243684\760\remote\39150\screenshots"
    let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
    let sw = new System.Diagnostics.Stopwatch()
    while true do
        printfn "Waiting for new screenshot..."
        let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
        printfn "#######################################"
        printfn "#### DETECTED %s" changedResult.Name
        printfn "#######################################"
        sw.Restart()

        let screenshot = new Bitmap(steamScreenshotDir + @"\" + changedResult.Name)
        let state = readGameState screenshot
        screenshot.Dispose()
        printfn "GameState read in %d ms" sw.ElapsedMilliseconds

        playGame state

    watcher.Dispose()

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    watchScreenshotDir()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
