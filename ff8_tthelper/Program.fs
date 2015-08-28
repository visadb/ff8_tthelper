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
    //Bootstrap.saveResultDetectionBitmaps()
    //Bootstrap.saveSpoilsSelectionNumberBitmaps()
    Bootstrap.saveCardChoosingScreenCardSymbolBitmap()
    ()

let printState state =
    printfn "%O-------------------------------------- v=%d b=%d\n" state (AI.evaluateNode state) (AI.cardBalance state)

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
    let sw = System.Diagnostics.Stopwatch()
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

    sw.Restart()
    let (srcHandIndex, targetGridIndex), value = AI.getBestMove state 9
    let took = sw.ElapsedMilliseconds
    printState state
    printfn "Best move is %d -> (%d,%d) with value %d (took %d ms)" (srcHandIndex) (targetGridIndex/3)
                                                                    (targetGridIndex%3) value took
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
    printfn "pressed"
    watcher.Dispose()

let watchScreenshotDir () =
    while true do
        let watcher = new System.IO.FileSystemWatcher(steamScreenshotDir, "????-??-??_?????.jpg")
        printfn "\nWaiting for new screenshot..."
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
    sendKey "F12"
    let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
    Thread.Sleep 100
    let filename = steamScreenshotDir + @"\" + changedResult.Name
    try
        let ss = SimpleBitmap.fromFile(filename)
        System.IO.File.Delete(filename)
        ss
    with
        | _ -> // retry
            Thread.Sleep 100
            let ss = SimpleBitmap.fromFile(filename)
            System.IO.File.Delete(filename)
            ss

let chooseCards() = 
    printfn "Choosing cards"

    sendAndSleep "Left" 150
    sendAndSleep "Up" 20

    // put cursor on last card on page
    let sw = System.Diagnostics.Stopwatch()
    sw.Restart()
    let numCardsOnPage = readNumberOfCardsOnCardChoosingScreen (takeScreenshot())
    printfn "Detected %d cards on page (took %d ms)" numCardsOnPage <| sw.ElapsedMilliseconds
    for i in 1 .. (11-numCardsOnPage) do
        sendAndSleep "Up" 20

    let cardsToTakeFromLastPage = min 5 numCardsOnPage
    for i in 1 .. cardsToTakeFromLastPage do
        sendAndSleep "x" 20
        if i <> cardsToTakeFromLastPage then
            sendAndSleep "Up" 20

    if numCardsOnPage < 5 then
        sendAndSleep "Left" 150
        sendAndSleep "Up" 20
        for i in 1 .. 5-numCardsOnPage do
            sendAndSleep "x" 20
            if i <> (5-numCardsOnPage) then
                sendAndSleep "Up" 20

    Thread.Sleep 500
        sendAndSleep "x" 1800
        printfn "Cards chosen!"

let startGame() =
    printfn "Starting game"
    sendAndSleep "s" 700 // Play game?
    sendAndSleep "x" 2000 // Yes
    sendAndSleep "x" 2500 // Talking
    sendAndSleep "x" 1700 // Rules
    chooseCards()

let rec playMatch() =
    let mutable lastScreenshot = takeScreenshot()
    while readGamePhase lastScreenshot = Ongoing do
        let mutable state = readGameState lastScreenshot
        while state.turnPhase = OpponentsTurn && readGamePhase lastScreenshot = Ongoing do
            printfn "Waiting for my turn..."
            Thread.Sleep 1000
            lastScreenshot <- takeScreenshot()
            state <- readGameState lastScreenshot
        if readGamePhase lastScreenshot = Ongoing then
            printfn "My turn now, playing!"
            playOneTurn state
            Thread.Sleep 2800
            lastScreenshot <- takeScreenshot()
    let result = readGamePhase lastScreenshot
    printfn "Game ended, result: %A" result
    Thread.Sleep 500
    sendAndSleep "x" 2000 // Dismiss Won/Draw/Lost screen
    if result = Draw then
        playMatch()

let chooseSpoils() =
    let spoilsSelectionNumber = readSpoilsSelectionNumber (takeScreenshot())
    if spoilsSelectionNumber.IsSome then
        printfn "Choosing %d cards for spoils" spoilsSelectionNumber.Value
        if spoilsSelectionNumber.Value = 1 then
            // Randomize chosen card
            for i in 1 .. System.Random().Next() % 5 do
                sendAndSleep "Right" 80
        for i in 1 .. spoilsSelectionNumber.Value do
            sendAndSleep "x" 400
            sendAndSleep "Right" 80
        sendAndSleep "x" 1300
        for i in 1 .. spoilsSelectionNumber.Value do
            sendAndSleep "x" 1300
    else
        printfn "Nothing to do, waiting for game to end..."
        Thread.Sleep 10000

let autoPlay() =
    // assert/assume that outside
    while true do
        startGame()
        playMatch()
        chooseSpoils()

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

    //bootstrap()

    //watchScreenshotDir()
    //playScreenshot <| screenshotDir + @"in-game\example_screenshot_4.jpg"
    //playScreenshot <| steamScreenshotDir + @"\2015-08-16_00001.jpg"
    //playTestState()

    waitForUserToPressF12()
    autoPlay()

    //while true do
    //    waitForUserToPressF12()
    //    startGame()
    //    playMatch()
    //    chooseSpoils()

    //while true do
    //    waitForUserToPressF12()
    //    let state = readGameState <| takeScreenshot()
    //    playGame state

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
