module GameStateDetectionTest

open FsUnit
open NUnit.Framework
open System.Drawing

open BitmapHelpers
open DomainTypes
open GameStateDetection

open TestHelpers

let screenshotGameStates =

    [
    (@"in-game\example_screenshot_1.jpg", {
        turnPhase = MyCardSelection 0
        myHand = [|hc [9;9;5;2] Me e; hc [5;9;1;9] Me e; hc [9;8;6;2] Me f; hc [1;7;8;7] Me n; hc [8;4;8;5] Me n|]
        opHand = [|None;              hc [4;7;6;2] Op n; hc [2;7;3;6] Op n; hc [6;5;5;4] Op n; hc [3;6;7;3] Op n|]
        playGrid = PlayGrid([| pc [1;7;6;4] Op 0; emptySlot; emptySlotElem e
                               emptySlot;         emptySlot; emptySlot
                               emptySlot;         emptySlot; emptySlot       |])
        })
    (@"in-game\example_screenshot_2.jpg", {
        turnPhase = MyCardSelection 4
        myHand = [|None; None; None; None; hc [9;9;5;2] Me e|]
        opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
        playGrid = PlayGrid([| pc [5;4;5;7] Op -1; pc [1;7;8;7] Me 0; pc [1;7;6;4] Op 0
                               pc [7;8;7;2] Me  0; pc [4;7;6;2] Me 0; pc [5;9;1;9] Me 0
                               pc [8;4;8;5] Me  0; emptySlot;         pc [9;8;6;2] Me 0 |])
        })
    (@"in-game\example_screenshot_3.jpg", {
        turnPhase = MyCardSelection 1
        myHand = [|None; hc [9;8;6;2] Me f; hc [8;8;5;2] Me t; hc [1;3;8;8] Me p; hc [7;4;8;3] Me w|]
        opHand = [|None; None             ; hc [7;8;7;2] Op n; hc [3;6;7;3] Op n; hc [7;4;2;7] Op f|]
        playGrid = PlayGrid([| pc [6;5;8;4] Op -1; emptySlotElem h; emptySlot
                               emptySlot;          emptySlot;       pc [9;9;5;2] Op +1
                               emptySlot;          emptySlot;       pc [7;6;3;1] Op +0 |])
        })
    (@"in-game\elemental_-1_in_0_1.jpg", {
        turnPhase = MyCardSelection 1
        myHand = [|None; hc [9;9;5;2] Me e; hc [9;8;6;2] Me f; hc [1;7;8;7] Me n; hc [8;4;8;5] Me n|]
        opHand = [|None; None;              hc [1;1;5;4] Op n; hc [6;5;8;4] Op n; hc [8;2;2;8] Op n|]
        playGrid = PlayGrid([| pc [4;6;5;5] Op  0; emptySlot; pc [5;9;1;9] Me 0
                               pc [5;3;1;1] Op -1; emptySlot; emptySlotElem p
                               emptySlot;          emptySlot; emptySlotElem p   |])
        })
    (@"in-game\elemental_+1_in_0_0.jpg", {
        turnPhase = OpponentsTurn
        myHand = Array.empty
        opHand = Array.empty
        playGrid = PlayGrid.Empty
        })
]

let targetSelectionGameStates =
    let baseGameState = {
            turnPhase = MyCardSelection 4
            myHand = (snd screenshotGameStates.[0]).myHand
            opHand = [|hc [5;4;5;7] Op n; hc [4;7;6;2] Op n; hc [1;7;6;4] Op t; hc [7;8;7;2] Op n; hc [1;5;3;3] Op n|]
            playGrid = PlayGrid([| emptySlotElem h; emptySlot; emptySlot
                                   emptySlot      ; emptySlot; emptySlot
                                   emptySlot      ; emptySlot; emptySlot |])
        }
    [ for y in 0..2 do
        for x in 0..2 ->
            (sprintf @"in-game\target_selection_%d_%d.jpg" x y,
               { baseGameState with turnPhase = MyTargetSelection (4, (x,y)) })]

let emptyPlayGridSlotElementTestData = [|
    (02, array2D [[n;n;n]; [f;n;p]; [n;n;p]])
    (03, array2D [[n;n;n]; [f;n;p]; [n;n;p]])
    (04, array2D [[n;n;n]; [f;n;p]; [n;n;p]])
    (10, array2D [[n;n;n]; [t;n;e]; [p;n;n]])
    (11, array2D [[n;n;n]; [t;n;e]; [p;n;n]])
    (13, array2D [[n;n;w]; [n;n;n]; [n;n;a]])
    (14, array2D [[n;n;w]; [n;n;n]; [n;n;a]])
    (15, array2D [[n;n;w]; [n;n;n]; [n;n;a]])
    (24, array2D [[f;i;n]; [n;n;n]; [w;n;n]])
    (25, array2D [[f;i;n]; [n;n;n]; [w;n;n]])
    (26, array2D [[f;i;n]; [n;n;n]; [w;n;n]])
    |]

let gameStateReadCorrectly (screenshotFile, expectedGameState): unit =
    let screenshot = SimpleBitmap.fromFile(screenshotDir + screenshotFile)
    readGameState screenshot |> should equal expectedGameState

let emptyPlayGridSlotElementsReadCorrectly (ssNum, expectedElems: Element option [,]): unit =
    let screenshot = SimpleBitmap.fromFile(sprintf @"%sin-game\elements\elements_%02d.jpg" screenshotDir ssNum)
    (readGameStateWithTurnPhase (Some (MyCardSelection 1)) screenshot).playGrid.Slots
        |> Array.iteri (fun i slot ->
            match slot with
                | Empty e -> sprintf "%A" e |> should equal (sprintf "%A" expectedElems.[i/3,i%3])
                | _ -> ()
            )

let gamePhaseReadCorrectly expectedGamePhase =
     let f = sprintf @"%sgetting_out\result_%s.jpg" screenshotDir ((sprintf "%A" expectedGamePhase).ToLower())
     readGamePhase (SimpleBitmap.fromFile(f)) |> should equal expectedGamePhase

let numberOfCardsOnCardChoosingScreenReadCorrectly screenshotFilename num =
    let ss = SimpleBitmap.fromFile(screenshotDir + screenshotFilename)
    readNumberOfCardsOnCardChoosingScreen ss |> should equal num

let ruleData = [
        ("rules_open_sudden_random_sameplus_elemental_one.jpg",
         Rules.having [Elemental; Open; Same; Plus; Random; SuddenDeath; TradeOne])
        ("rules_open_sudden_elemental_diff.jpg",
         Rules.having [Open; SuddenDeath; Elemental; TradeDiff])
        ("rules_random_plus_elemental_one.jpg",
         Rules.having [Random; Plus; Elemental; TradeOne])
        ("rules_open_sudden_sameplus_elemental_diff.jpg",
         Rules.having [Open; SuddenDeath; Same; Plus; Elemental; TradeDiff])
    ]

let rulesReadCorrectly (ruleScreenshotFilename, expectedRules) =
    let rules = readRules <| SimpleBitmap.fromFile(screenshotDir + @"getting_in\" + ruleScreenshotFilename)
    rules |> should equal expectedRules

[<TestFixture>]
type ``Game state detector test`` ()=

    [<Test>] member x.
     ``Game state is read correctly from example screenshots`` ()=
        screenshotGameStates |> List.iter gameStateReadCorrectly

    [<Test>] member x.
     ``Target selection game states read correctly`` ()=
        targetSelectionGameStates |> List.iter gameStateReadCorrectly

    [<Test>] member x.
     ``Empty grid slot elements read correctly`` ()=
        emptyPlayGridSlotElementTestData |> Array.iter emptyPlayGridSlotElementsReadCorrectly

    [<Test>] member x.
     ``Game phase read correctly`` ()=
        [Won; Draw; Lost] |> List.iter gamePhaseReadCorrectly

    [<Test>] member x.
     ``Number of cards on card choosing screen read correctly`` ()=
        numberOfCardsOnCardChoosingScreenReadCorrectly @"getting_in\card_selection_page1.jpg" 11;
        numberOfCardsOnCardChoosingScreenReadCorrectly @"getting_in\card_selection_page7.jpg" 9

    [<Test>] member x.
     ``Rules read correctly`` ()=
        ruleData |> List.iter rulesReadCorrectly