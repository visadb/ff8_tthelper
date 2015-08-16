module GameStateDetectionTest

open FsUnit
open NUnit.Framework
open System.Drawing

open DomainTypes
open GameStateDetection

let card powers owner powerModifier element =
    Some { powers = List.toArray powers; powerModifier = powerModifier
           element = element; owner = owner}
let hc powers owner = card powers owner 0
let pc powers owner powerModifier = Full (card powers owner powerModifier (Some Unknown)).Value
let emptySlot = PlayGridSlot.Empty None
let emptySlotElem elem = PlayGridSlot.Empty (Some elem)

let n = None
let e = Some Earth
let f = Some Fire
let p = Some Poison
let t = Some Thunder
let w = Some Wind

let screenshotGameStates = 
    
    [
    (@"in-game\example_screenshot_1.jpg", {
        turnPhase = MyCardSelection 0
        myHand = [|hc [9;9;5;2] Me e; hc [5;9;1;9] Me e; hc [9;8;6;2] Me f; hc [1;7;8;7] Me n; hc [8;4;8;5] Me n|]
        opHand = [|None;              hc [4;7;6;2] Op n; hc [2;7;3;6] Op n; hc [6;5;5;4] Op n; hc [3;6;7;3] Op n|]
        playGrid = { slots = (array2D [ [ pc [1;7;6;4] Op 0; emptySlot; emptySlotElem Unknown ]
                                        [ emptySlot;         emptySlot; emptySlot ]
                                        [ emptySlot;         emptySlot; emptySlot ] ])}
        })
    (@"in-game\example_screenshot_2.jpg", {
        turnPhase = MyCardSelection 4
        myHand = [|None; None; None; None; hc [9;9;5;2] Me e|]
        opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
        playGrid = { slots = (array2D [ [ pc [5;4;5;7] Op -1; pc [1;7;8;7] Me 0; pc [1;7;6;4] Op 0 ]
                                        [ pc [7;8;7;2] Me  0; pc [4;7;6;2] Me 0; pc [5;9;1;9] Me 0 ]
                                        [ pc [8;4;8;5] Me  0; emptySlot;         pc [9;8;6;2] Me 0 ] ])}
        })
    (@"in-game\example_screenshot_3.jpg", {
        turnPhase = MyCardSelection 1
        myHand = [|None; hc [9;8;6;2] Me f; hc [8;8;5;2] Me t; hc [1;3;8;8] Me p; hc [7;4;8;3] Me w|]
        opHand = [|None; None             ; hc [7;8;7;2] Op n; hc [3;6;7;3] Op n; hc [7;4;2;7] Op f|]
        playGrid = { slots = (array2D [ [ pc [6;5;8;4] Op -1; emptySlotElem Unknown; emptySlot          ]
                                        [ emptySlot;          emptySlot;             pc [9;9;5;2] Op +1 ]
                                        [ emptySlot;          emptySlot;             pc [7;6;3;1] Op +0 ] ])}
        })
    (@"in-game\elemental_-1_in_0_1.jpg", {
        turnPhase = MyCardSelection 1
        myHand = [|None; hc [9;9;5;2] Me e; hc [9;8;6;2] Me f; hc [1;7;8;7] Me n; hc [8;4;8;5] Me n|]
        opHand = [|None; None;              hc [1;1;5;4] Op n; hc [6;5;8;4] Op n; hc [8;2;2;8] Op n|]
        playGrid = { slots = (array2D [ [ pc [4;6;5;5] Op  0; emptySlot; pc [5;9;1;9] Me 0     ]
                                        [ pc [5;3;1;1] Op -1; emptySlot; emptySlotElem Unknown ]
                                        [ emptySlot;          emptySlot; emptySlotElem Unknown ] ])}
        })
    (@"in-game\elemental_+1_in_0_0.jpg", {
        turnPhase = OpponentsTurn
        myHand = Array.empty
        opHand = Array.empty
        playGrid = { slots = array2D [] }
        })
]

let targetSelectionGameStates =
    let baseGameState = {
            turnPhase = MyCardSelection 4
            myHand = (snd screenshotGameStates.[0]).myHand
            opHand = [|hc [5;4;5;7] Op n; hc [4;7;6;2] Op n; hc [1;7;6;4] Op t; hc [7;8;7;2] Op n; hc [1;5;3;3] Op n|]
            playGrid = { slots = (array2D [ [ emptySlotElem Unknown; emptySlot; emptySlot ]
                                            [ emptySlot            ; emptySlot; emptySlot ]
                                            [ emptySlot            ; emptySlot; emptySlot ] ])}
        }
    [ for y in 0..2 do
        for x in 0..2 ->
            (sprintf @"in-game\target_selection_%d_%d.jpg" x y,
               { baseGameState with turnPhase = MyTargetSelection (4, (x,y)) })]

let gameStateReadCorrectly (screenshotFile, expectedGameState): unit =
    let screenshot = new Bitmap(screenshotDir + screenshotFile)
    readGameState screenshot |> should equal expectedGameState
    screenshot.Dispose()
    

[<TestFixture>]
type ``Game state detector test`` ()=
    
    [<Test>] member x.
     ``Game state is read correctly from example screenshots`` ()=
        screenshotGameStates |> List.iter gameStateReadCorrectly
    
    [<Test>] member x.
     ``Target selection game states read correctly`` ()=
        targetSelectionGameStates |> List.iter gameStateReadCorrectly