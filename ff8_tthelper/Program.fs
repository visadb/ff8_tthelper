open GameStateDetection

open DomainTypes
open BitmapHelpers
open GameStateDetectionTest

open System.Drawing
open System.Threading


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
let sendAndSleep (key: string) (ms: int) =
    sendKey key
    Thread.Sleep ms

let clearScript = System.IO.Directory.GetCurrentDirectory() + @"\..\..\clearScreenshots.ahk"
let clearSteamScreenshots() = 
    let proc = System.Diagnostics.Process.Start(ahkProg, clearScript)
    proc.WaitForExit()

let playOneTurn state =
    let sw = System.Diagnostics.Stopwatch()
    let selectHandCard offset = 
        printfn "Selecting hand card: %d" offset
        let key = if offset < 0 then "Up" else "Down"
        for i in 1 .. abs offset do
            sendAndSleep key 20
        sendAndSleep "x" 20
    let selectTargetSlot rowOffset colOffset =
        printfn "Selecting target slot: (%d, %d)" rowOffset colOffset
        if rowOffset = -1 then sendAndSleep "Up" 20
        elif rowOffset = 1 then sendAndSleep "Down" 20
        if colOffset = -1 then sendAndSleep "Left" 20
        elif colOffset = 1 then sendAndSleep "Right" 20
        sendAndSleep "x" 20

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

let waitForScreenshot() =
    let watcher = new System.IO.FileSystemWatcher(screenCaptureDir, screenshotFilePattern)
    watcher.InternalBufferSize <- 1000000
    let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
    watcher.Dispose()
    screenCaptureDir + @"\" + changedResult.Name

let waitForUserToPressScreenshotHotkey() =
    printfn "Press %s inside game" screenshotHotKey
    waitForScreenshot() |> ignore
    printfn "pressed"

let playOneTurnAtATime() =
    while true do
        let watcher = new System.IO.FileSystemWatcher(screenCaptureDir, screenshotFilePattern)
        printfn "\nWaiting for new screenshot..."
        let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created)
        watcher.Dispose()
        printfn "# DETECTED %s" changedResult.Name
        let state = readGameStateFromScreenshot(screenCaptureDir + @"\" + changedResult.Name)
        if state.turnPhase = OpponentsTurn then
            printfn "Not my turn..."
        else
            printfn "My turn, playing!"
            playOneTurn state

let rec takeScreenshot(): SimpleBitmap =
    let watcher = new System.IO.FileSystemWatcher(screenCaptureDir, screenshotFilePattern)
    sendKey screenshotHotKey
    let changedResult = watcher.WaitForChanged(System.IO.WatcherChangeTypes.Created, 10000)
    if changedResult.TimedOut then
        printfn "Timed out while waiting for screenshot, retrying..."
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
    sendAndSleep "x" 2000 // Talking
    sendAndSleep "x" 1700 // Rules
    chooseCards()

let rec playMatch() =
    let mutable lastScreenshot = takeScreenshot()
    while readGamePhase lastScreenshot = Ongoing do
        let mutable state = readGameState lastScreenshot
        while state.turnPhase = OpponentsTurn && readGamePhase lastScreenshot = Ongoing do
            printfn "Waiting for my turn..."
            Thread.Sleep 2500
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
    waitForUserToPressScreenshotHotkey()
    // assert/assume that outside
    for i in 1 .. 2000000000 do
        startGame()
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

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
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

    autoPlay()
    //playOneTurnAtATime()

    sw.Stop()
    printfn "Time elapsed: %d ms" sw.ElapsedMilliseconds

    0
