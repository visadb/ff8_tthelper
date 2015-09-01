open GameStateDetection

open DomainTypes
open BitmapHelpers
open GameStateDetectionTest
open System
open Printf

open System.Drawing
open System.Threading

let logFile = @"C:\tmp\ff8helper_log.txt"
let logStream = new IO.StreamWriter(logFile, true)

let log msg =
    let toWrite = (DateTime.Now.ToString()) + ": " + msg
    logStream.WriteLine(toWrite)
    logStream.Flush()
    Console.WriteLine(toWrite)
    

//let screenCaptureDir = @"C:\tmp\fraps_screenshots"
//let screenshotFilePattern = "*.jpg"
//let screenshotHotKey = "F11"
let screenCaptureDir = @"D:\Program Files\Steam\userdata\33243684\760\remote\39150\screenshots"
let screenshotFilePattern = "????-??-??_?????.jpg"
let screenshotHotKey = "F12"

let bootstrap () =
    //Bootstrap.savePowerModifiersFromExampleScreenshots()
    //Bootstrap.saveCursorFromExampleScreenshot()
    Bootstrap.saveDigitFilesFromExampleScreenshot()
    Bootstrap.printDiffs()
    //Bootstrap.saveElementSymbolsFromExampleScreenshots()
    //Bootstrap.saveEmptyElementlessPlayGridSlotElementBitmaps()
    //Bootstrap.saveEmptyPlayGridSlotElementBitmaps()
    //Bootstrap.saveResultDetectionBitmaps()
    //Bootstrap.saveSpoilsSelectionNumberBitmaps()
    //Bootstrap.saveCardChoosingScreenCardSymbolBitmap()
    ()

let printState state =
    ksprintf log "%O-------------------------------------- v=%d b=%d\n" state (AI.evaluateNode state) (AI.cardBalance state)

let playGame initState =
    let mutable state = initState
    let sw = new Diagnostics.Stopwatch()
    while not (AI.isTerminalNode state) do
        printState state
        sw.Restart()
        let (move, value) = AI.getBestMove state 9
        ksprintf log "Best move is %d -> (%d,%d) with value %d (took %d ms)" (fst move)
                                                                        (snd move / 3) (snd move % 3)
                                                                        value
                                                                        sw.ElapsedMilliseconds
        state <- AI.executeMove state move

    printState state

let readGameStateFromScreenshot (screenshotPath: string) =
    let sw = new Diagnostics.Stopwatch()
    ksprintf log "Reading GameState from %s" screenshotPath
    sw.Restart()
    let screenshot = SimpleBitmap.fromFile(screenshotPath)
    let state = readGameState screenshot
    ksprintf log "GameState read in %d ms" sw.ElapsedMilliseconds
    state

let ahkProg = @"C:\Program Files\AutoHotkey\AutoHotkey.exe"
let sendScript = IO.Directory.GetCurrentDirectory() + @"\..\..\send.ahk"
let sendKey key =
    let proc = Diagnostics.Process.Start(ahkProg, sendScript+" "+key)
    proc.WaitForExit()
let sendAndSleep (key: string) (ms: int) =
    sendKey key
    Thread.Sleep ms

let clearScript = IO.Directory.GetCurrentDirectory() + @"\..\..\clearScreenshots.ahk"
let clearSteamScreenshots() = 
    ksprintf log "Clearing Steam screenshots"
    let proc = Diagnostics.Process.Start(ahkProg, clearScript)
    proc.WaitForExit()

let playOneTurn state =
    let sw = Diagnostics.Stopwatch()
    let selectHandCard offset = 
        ksprintf log "Selecting hand card: %d" offset
        let key = if offset < 0 then "Up" else "Down"
        for i in 1 .. abs offset do
            sendAndSleep key 20
        sendAndSleep "x" 20
    let selectTargetSlot rowOffset colOffset =
        ksprintf log "Selecting target slot: (%d, %d)" rowOffset colOffset
        if rowOffset = -1 then sendAndSleep "Up" 20
        elif rowOffset = 1 then sendAndSleep "Down" 20
        if colOffset = -1 then sendAndSleep "Left" 20
        elif colOffset = 1 then sendAndSleep "Right" 20
        sendAndSleep "x" 20

    sw.Restart()
    let (srcHandIndex, targetGridIndex), value = AI.getBestMove state 9
    let took = sw.ElapsedMilliseconds
    printState state
    ksprintf log "Best move is %d -> (%d,%d) with value %d (took %d ms)" (srcHandIndex) (targetGridIndex/3)
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

let waitForScreenshot() =
    let watcher = new IO.FileSystemWatcher(screenCaptureDir, screenshotFilePattern)
    try
        watcher.InternalBufferSize <- 1000000
        let changedResult = watcher.WaitForChanged(IO.WatcherChangeTypes.Created)
        screenCaptureDir + @"\" + changedResult.Name
    finally
        watcher.Dispose()

let waitForUserToPressScreenshotHotkey() =
    ksprintf log "Press %s inside game" screenshotHotKey
    waitForScreenshot() |> ignore
    ksprintf log "pressed"

let playOneTurnAtATime() =
    while true do
        let screenshotFilename = waitForScreenshot()
        let state = readGameStateFromScreenshot screenshotFilename
        if state.turnPhase = OpponentsTurn then
            ksprintf log "Not my turn..."
        else
            ksprintf log "My turn, playing!"
            playOneTurn state

let rec takeScreenshot(): SimpleBitmap =
    let watcher = new IO.FileSystemWatcher(screenCaptureDir, screenshotFilePattern)
    try
        watcher.InternalBufferSize <- 1000000
        sendKey screenshotHotKey
        let changedResult = watcher.WaitForChanged(IO.WatcherChangeTypes.Created, 10000)
        if changedResult.TimedOut then
            ksprintf log "Timed out while waiting for screenshot, retrying..."
            takeScreenshot() // retry indefinitely
        else
            Thread.Sleep 100
            let filename = screenCaptureDir + @"\" + changedResult.Name
            try
                let ss = SimpleBitmap.fromFile(filename)
                ss
            with
                | _ -> // retry once
                    Thread.Sleep 100
                    let ss = SimpleBitmap.fromFile(filename)
                    ss
    finally
        watcher.Dispose()

let chooseCards() = 
    ksprintf log "Choosing cards"

    sendAndSleep "Left" 150
    sendAndSleep "Up" 20

    // put cursor on last card on page
    let sw = Diagnostics.Stopwatch()
    sw.Restart()
    let numCardsOnPage = readNumberOfCardsOnCardChoosingScreen (takeScreenshot())
    ksprintf log "Detected %d cards on page (took %d ms)" numCardsOnPage <| sw.ElapsedMilliseconds
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

    Thread.Sleep 1000
    sendAndSleep "x" 1800
    ksprintf log "Cards chosen!"

let startGame i =
    ksprintf log "Starting game %d" i
    sendAndSleep "s" 700 // Play game?
    sendAndSleep "x" 2000 // Yes
    sendAndSleep "x" 2000 // Talking
    sendAndSleep "x" 1700 // Rules
    chooseCards()

let rec playMatch() =
    let mutable lastScreenshot = takeScreenshot()
    while readGamePhase lastScreenshot = Ongoing do
        let mutable state = readGameState lastScreenshot
        while state.turnPhase = OpponentsTurn && readGamePhase lastScreenshot = Ongoing do
            ksprintf log "Waiting for my turn..."
            Thread.Sleep 2500
            lastScreenshot <- takeScreenshot()
            state <- readGameState lastScreenshot
        if readGamePhase lastScreenshot = Ongoing then
            ksprintf log "My turn now, playing!"
            playOneTurn state
            Thread.Sleep 2800
            lastScreenshot <- takeScreenshot()
    let result = readGamePhase lastScreenshot
    ksprintf log "Game ended, result: %A" result
    Thread.Sleep 500
    sendAndSleep "x" 2000 // Dismiss Won/Draw/Lost screen
    if result = Draw then
        playMatch()

let chooseSpoils() =
    let spoilsSelectionNumber = readSpoilsSelectionNumber (takeScreenshot())
    if spoilsSelectionNumber.IsSome then
        ksprintf log "Choosing %d cards for spoils" spoilsSelectionNumber.Value
        if spoilsSelectionNumber.Value = 1 then
            // Randomize chosen card
            for i in 1 .. Random().Next() % 5 do
                sendAndSleep "Right" 80
        for i in 1 .. spoilsSelectionNumber.Value do
            sendAndSleep "x" 400
            sendAndSleep "Right" 80
        sendAndSleep "x" 1300
        for i in 1 .. spoilsSelectionNumber.Value do
            sendAndSleep "x" 1300
    else
        ksprintf log "Nothing to do, waiting for game to end..."
        Thread.Sleep 10000

let autoPlay() =
    waitForUserToPressScreenshotHotkey()
    // assert/assume that outside
    for i in 0 .. 2000000000 do
        startGame i
        playMatch()
        chooseSpoils()
        if i % 10 = 0 then
            clearSteamScreenshots()

let playScreenshot (screenshotPath: string) =
    playGame <| readGameStateFromScreenshot screenshotPath
    
let playTestState() =
    let state = {
            turnPhase = OpponentsTurn
            myHand = [|None; None; None; None             ; hc [1;2;1;1] Me n|]
            opHand = [|None; None; None; hc [1;1;1;1] Op n; hc [1;1;2;1] Op n|]
            playGrid = PlayGrid([| pc [1;1;1;1] Me 0; pc [1;1;1;1] Op 0; pc [1;1;1;1] Me 0
                                   pc [1;1;2;1] Op 0; emptySlot        ; pc [1;2;1;1] Me 0
                                   pc [1;1;1;1] Op 0; emptySlot        ; pc [1;1;1;1] Me 0 |])
    }
    playGame state

// TODO: DEBUG
// My turn now, playing!
// 
//        m1764  | o6631- | m7113
//        -------+--------+-------
//        m9952  | o7365  | m5919
//        -------+--------+------- @9239
// 5637          |        | m9862-  7872
// -------------------------------------- v=55 b=4
//
// Best move is 4 -> (2,0) with value 4 (took 0 ms)
//
//        m1764  | o6631- | m7113
//        -------+--------+-------
//        m9952  | o7365  | m5919
//        -------+--------+-------
// 5637   m7872  |        | m9862-  9239
// -------------------------------------- v=70 b=4

[<EntryPoint>]
let main argv = 
    let sw = new Diagnostics.Stopwatch()
    sw.Start()

    //bootstrap()

    //watchScreenshotDir()
    //playScreenshot <| screenshotDir + @"in-game\example_screenshot_4.jpg"
    //playScreenshot <| screenshotDir + @"in-game\card_selection_cursor_1.jpg"
    //playScreenshot <| screenCaptureDir + @"\2015-08-16_00001.jpg"
    //playTestState()
        
    //let ss = SimpleBitmap.fromFile <| screenshotDir + @"in-game\misread_card_2.jpg"
    //let card = readCard ss None None None <| Point(616+240, 93+308)
    //printfn "Card: %A" card

    //while true do
    //    waitForUserToPressScreenshotHotkey()
    //    clearSteamScreenshots()
    //    Thread.Sleep 500

    //autoPlay()
    playOneTurnAtATime()

    sw.Stop()
    ksprintf log "Time elapsed: %d ms" sw.ElapsedMilliseconds

    0
