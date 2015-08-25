open GameStateDetection

open DomainTypes
open GameStateDetectionTest

open System.Drawing
open System.Threading

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

let printState state =
    printfn "%O-------------------------------------- v=%d b=%d\n" state (AI.evaluateNode state false) (AI.cardBalance state)

let playGame initState =
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

let ahkProg = @"C:\Program Files\AutoHotkey\AutoHotkey.exe"
let sendScript = System.IO.Directory.GetCurrentDirectory() + @"\..\..\send.ahk"
let sendKey key =
    let proc = System.Diagnostics.Process.Start(ahkProg, sendScript+" "+key)
    proc.WaitForExit()
    

let playOneTurn state =
    let selectHandCard offset = 
        printfn "Selecting hand card: %d" offset
        let key = if offset < 0 then "Up" else "Down"
        for i in 1 .. abs offset do
            sendKey key
        sendKey "x"
    let selectTargetSlot rowOffset colOffset =
        printfn "Selecting target slot: (%d, %d)" rowOffset colOffset
        if rowOffset = -1 then sendKey "Up"
        elif rowOffset = 1 then sendKey "Down"
        if colOffset = -1 then sendKey "Left"
        elif colOffset = 1 then sendKey "Right"
        sendKey "x"

    let (srcHandIndex, targetGridIndex), value = AI.getBestMove state 9
    printState state
    printfn "Best move is %d -> (%d,%d) with value %d" (srcHandIndex) (targetGridIndex/3)
                                                       (targetGridIndex%3) value
    printState <| AI.executeMove state (srcHandIndex, targetGridIndex)
    match state.turnPhase with
        | MyCardSelection currentHandIndex ->
            selectHandCard <| srcHandIndex - currentHandIndex
        | MyTargetSelection (currentHandIndex,_) ->
            sendKey "c"
            selectHandCard <| srcHandIndex - currentHandIndex
        | _ -> ()

    // Now selecting target, cursor at (1,1)
    let rowOffset, colOffset = targetGridIndex/3 - 1, targetGridIndex%3 - 1
    selectTargetSlot rowOffset colOffset

let waitForUserToPressF12() =
    let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
    printfn "Press F12 inside game"
    let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
    watcher.Dispose()

let watchScreenshotDir () =
    while true do
        let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
        printfn "Waiting for new screenshot..."
        let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
        watcher.Dispose()
        printfn "#######################################"
        printfn "#### DETECTED %s" changedResult.Name
        printfn "#######################################"
        let state = readGameStateFromScreenshot(steamScreenshotDir + @"\" + changedResult.Name)
        if state.turnPhase = OpponentsTurn then
            printfn "Not my turn..."
        else
            printfn "My turn, playing!"
            playOneTurn state

let sendAndSleep (key: string) (ms: int) =
    sendKey key
    Thread.Sleep ms

let takeScreenshot(): SimpleBitmap =
    let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
    printfn "Waiting screenshot taken by me..."
    sendKey "F12"
    let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
    printfn "Got %s" changedResult.Name
    Thread.Sleep 100
    try
        SimpleBitmap.fromFile(steamScreenshotDir + @"\" + changedResult.Name)
    with
        | _ -> // retry
            Thread.Sleep 100
            SimpleBitmap.fromFile(steamScreenshotDir + @"\" + changedResult.Name)

let chooseCards() = 
    sendAndSleep "Left" 200
    sendAndSleep "Up" 20

    let mutable triedCount = 0
    while (triedCount < 14)
          && ((triedCount < 5) || not (isAtCardSelectionConfirmationNo (takeScreenshot()))) do
        sendAndSleep "x" 400
        if triedCount > 0 && triedCount % 10 = 0 then
            sendAndSleep "Left" 200
        sendAndSleep "Up" 20
        triedCount <- triedCount + 1

    if triedCount = 15 then
        printfn "Card selection failed :(((("
    else
        sendAndSleep "Up" 30
        sendAndSleep "x" 2000
        printfn "Cards chosen!"


let startGame() =
    sendAndSleep "s" 700 // Play game?
    sendAndSleep "x" 2000 // Yes
    sendAndSleep "x" 2500 // Talking
    sendAndSleep "x" 2000 // Rules
    chooseCards()


let autoPlay() =
    // assert/assume that outside
    startGame()

let playScreenshot (screenshotPath: string) =
    playGame <| readGameStateFromScreenshot screenshotPath
    
let playTestState() =
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
    //playTestState()

    while true do
        waitForUserToPressF12()
        startGame()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
