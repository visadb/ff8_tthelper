open GameStateDetection
open DomainTypes
open BitmapHelpers
open TestHelpers

open System
open Printf

open System.Threading

let logFile = @"C:\tmp\ff8helper_log.txt"
let logStream = new IO.StreamWriter(logFile, true)

let log msg =
    let toWrite = (DateTime.Now.ToString()) + ": " + msg
    logStream.WriteLine(toWrite)
    logStream.Flush()
    Console.WriteLine(toWrite)
    
let screenCaptureDir = @"D:\Program Files\Steam\userdata\33243684\760\remote\39150\screenshots"
let screenshotFilePattern = "????-??-??_?????.jpg"
let screenshotHotKey = "F12"

let bootstrap () =
    Bootstrap.savePowerModifiersFromExampleScreenshots()
    Bootstrap.saveCursorFromExampleScreenshot()
    Bootstrap.saveDigitFilesFromExampleScreenshot()
    Bootstrap.printDiffs()
    Bootstrap.saveElementSymbolsFromExampleScreenshots()
    Bootstrap.saveEmptyElementlessPlayGridSlotElementBitmaps()
    Bootstrap.saveEmptyPlayGridSlotElementBitmaps()
    Bootstrap.saveResultDetectionBitmaps()
    Bootstrap.saveSpoilsSelectionNumberBitmaps()
    Bootstrap.saveCardChoosingScreenCardSymbolBitmap()
    Bootstrap.saveModelRuleBitmaps()
    ()

let printState state =
    ksprintf log "%O-------------------------------------- b=%d\r\n" state (AI.cardBalance state)

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

let playOneTurn (state: GameState) (rules: Rules) =
    let sw = Diagnostics.Stopwatch()
    let selectHandCard offset = 
        ksprintf log "Selecting hand card with offset %d" offset
        let key = if offset < 0 then "Up" else "Down"
        for i in 1 .. abs offset do
            sendAndSleep key 20
        sendAndSleep "x" 20
    let selectTargetSlot rowOffset colOffset =
        ksprintf log "Selecting target slot with offset (%d, %d)" rowOffset colOffset
        if rowOffset = -1 then sendAndSleep "Up" 20
        elif rowOffset = 1 then sendAndSleep "Down" 20
        if colOffset = -1 then sendAndSleep "Left" 20
        elif colOffset = 1 then sendAndSleep "Right" 20
        sendAndSleep "x" 20

    sw.Restart()
    let (srcHandIndex, targetGridIndex), value = AI.getBestMove state rules 9
    let took = sw.ElapsedMilliseconds
    printState state
    ksprintf log "Best move is %d -> (%d,%d) with value %d (took %d ms)" (srcHandIndex) (targetGridIndex/3)
                                                                         (targetGridIndex%3) value took
    printState <| AI.executeMove state rules (srcHandIndex, targetGridIndex)
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

let waitForScreenshotBitmap() =
    let mutable bitmap: SimpleBitmap option = None
    let mutable triesLeft = 5
    while bitmap.IsNone && triesLeft > 0 do
        let filename = waitForScreenshot()
        try
            bitmap <- Some (SimpleBitmap.fromFile(filename))
        with
            | _ -> triesLeft <- triesLeft - 1
                   ksprintf log "Failed to read screenshot"
    bitmap.Value

let waitForUserToPressScreenshotHotkey() =
    ksprintf log "\r\n--------Press %s inside game" screenshotHotKey
    waitForScreenshot() |> ignore
    ksprintf log "pressed"

let playOneTurnAtATime rules =
    while true do
        ksprintf log "\r\nTake screenshot to play one turn"
        let screenshotFilename = waitForScreenshot()
        let state = readGameStateFromScreenshot screenshotFilename
        if state.turnPhase = OpponentsTurn then
            ksprintf log "Not my turn..."
        else
            ksprintf log "My turn, calculating best move..."
            playOneTurn state rules

let mutable screenshotCount = 0
let rec takeScreenshot(): SimpleBitmap =
    //if (screenshotCount+1) % 100 = 0 then
    //    clearSteamScreenshots()
    let watcher = new IO.FileSystemWatcher(screenCaptureDir, screenshotFilePattern)

    // Take screenshot after 200ms when watcher is probably ready.
    let ssTimer = new System.Timers.Timer(200.0)
    ssTimer.Elapsed.Add (fun _ -> sendKey screenshotHotKey)
    ssTimer.AutoReset <- false
    ssTimer.Start()

    let changedResult = watcher.WaitForChanged(IO.WatcherChangeTypes.Created, 4000)
    watcher.Dispose()
    ssTimer.Close()
    if changedResult.TimedOut then
        ksprintf log "Timed out while waiting for screenshot, retrying..."
        takeScreenshot()
    else
        screenshotCount <- screenshotCount + 1
        Thread.Sleep 100
        let filename = screenCaptureDir + @"\" + changedResult.Name
        try
            SimpleBitmap.fromFile(filename)
        with
            | _ -> // retry once
                Thread.Sleep 100
                SimpleBitmap.fromFile(filename)

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
    sendAndSleep "x" 2500
    ksprintf log "Cards chosen!"

let rec playMatch (rules: Rules) =
    let mutable lastScreenshot = takeScreenshot()
    while readGamePhase lastScreenshot = Ongoing do
        let mutable state = readGameState lastScreenshot
        while state.turnPhase = OpponentsTurn && readGamePhase lastScreenshot = Ongoing do
            ksprintf log "Waiting for my turn..."
            Thread.Sleep 1000
            lastScreenshot <- takeScreenshot()
            state <- readGameState lastScreenshot
        if readGamePhase lastScreenshot = Ongoing then
            ksprintf log "My turn now, calculating best move..."
            playOneTurn state rules
            Thread.Sleep 1500
            lastScreenshot <- takeScreenshot()
    let result = readGamePhase lastScreenshot
    ksprintf log "Game ended, result: %A" result
    Thread.Sleep 500
    sendAndSleep "x" 2000 // Dismiss Won/Draw/Lost screen
    if result = Draw then
        playMatch rules

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
        Thread.Sleep 2000
    else
        ksprintf log "Nothing to do, waiting for game to end..."
        Thread.Sleep 10000

let startGameAgainstThatSittingDude i =
    ksprintf log "--------------------------------------------------------"
    ksprintf log "Starting game %d" i
    sendAndSleep "s" 700 // Play game?
    sendAndSleep "x" 2000 // Yes
    sendAndSleep "x" 2000 // Talking
    sendAndSleep "x" 1700 // Rules
    chooseCards()

let autoPlayAgainstThatSittingDude rules =
    waitForUserToPressScreenshotHotkey()
    // assert/assume that outside
    for i in 0 .. 2000000000 do
        startGameAgainstThatSittingDude i
        playMatch rules
        chooseSpoils()

let playOneGameAtATimeStartingFromRulesScreen() =
    while true do
        ksprintf log "Take screenshot in rules screen to play one game"
        let rules = waitForScreenshotBitmap() |> readRules
        if not rules.isValidRuleSet then
            ksprintf log "Invalid rule set: %A" rules.rules
        else
            ksprintf log "Read valid rules: %A" rules.rules
            sendAndSleep "x" 1700 // dismiss rules
            if not (rules.has DomainTypes.Random) then
                chooseCards()
            playMatch rules

let playGame initState rules =
    let mutable state = initState
    let sw = new Diagnostics.Stopwatch()
    while not (AI.isTerminalNode state) do
        ksprintf log "Game state before move:"
        printState state
        sw.Restart()
        let (move, value) = AI.getBestMove state rules 9
        ksprintf log "Best move is %d -> (%d,%d) with value %d (took %d ms)" (fst move)
                                                                             (snd move / 3) (snd move % 3)
                                                                             value
                                                                             sw.ElapsedMilliseconds
        state <- AI.executeMove state rules move

    printState state

let playScreenshot (screenshotPath: string) rules =
    playGame (readGameStateFromScreenshot screenshotPath) rules
    
let testState = {
        turnPhase = MyCardSelection 4
        myHand = [|None; None; None; None             ; hc [9;8;6;2] Me n|]
        opHand = [|None; None; None; None             ; hc [5;6;3;7] Op n|]
        playGrid = PlayGrid([| pc [9;9;5;2] Op -1; pc [6;6;3;1] Op -1; pc [7;1;1;3] Op 0
                               pc [9;9;5;2] Op  0; pc [5;9;1;9] Op  0; pc [1;7;6;4] Op 0
                               pc [8;4;8;5] Me  0; emptySlot         ; pc [1;7;8;7] Op 0 |])
}

[<EntryPoint>]
let main argv = 
    let sw = new Diagnostics.Stopwatch()
    sw.Start()

    // Choose mode by uncommenting one line below:
    //bootstrap()
    //playScreenshot <| screenshotDir + @"in-game\card_with_power_a.jpg"
    //playGame testState { Rules.none with isSame = true }
    //playOneTurnAtATime()
    //autoPlayAgainstThatSittingDude Rules.none
    playOneGameAtATimeStartingFromRulesScreen()

    sw.Stop()
    ksprintf log "Time elapsed: %d ms" sw.ElapsedMilliseconds

    0
