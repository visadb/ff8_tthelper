module GameStateDetectionTest

open FsUnit
open NUnit.Framework
open System.Drawing

open DomainTypes
open GameStateDetection

let card powers owner = Some { powers = List.toArray powers ; powerModifier = 0; element = None; owner = owner}
let slot powers owner = Full (card powers owner).Value
let emptySlot = PlayGridSlot.Empty None


let screenshotGameStates = [
    (@"in-game\example_screenshot_1.jpg", {
        turnPhase = MyCardSelection 0
        myHand = [| card [9;9;5;2] Me; card [5;9;1;9] Me; card [9;8;6;2] Me; card [1;7;8;7] Me; card [8;4;8;5] Me |]
        opponentsHand = [| Option.None; card [4;7;6;2] Op; card [2;7;3;6] Op; card [6;5;5;4] Op; card [3;6;7;3] Op |]
        playGrid = { slots = (array2D [ [ slot [1;7;6;4] Op; emptySlot; emptySlot ]
                                        [ emptySlot;         emptySlot; emptySlot ]
                                        [ emptySlot;         emptySlot; emptySlot ] ])}
        })
    (@"in-game\example_screenshot_2.jpg", {
        turnPhase = MyCardSelection 4
        myHand = [| Option.None; Option.None; Option.None; Option.None; card [9;9;5;2] Me |]
        opponentsHand = [| Option.None; Option.None; Option.None; Option.None; card [1;5;3;3] Op |]
        playGrid = { slots = (array2D [ [ slot [5;4;5;7] Op; slot [1;7;8;7] Me; slot [1;7;6;4] Op ]
                                        [ slot [7;8;7;2] Me; slot [4;7;6;2] Me; slot [5;9;1;9] Me ]
                                        [ slot [8;4;8;5] Me; emptySlot;         slot [9;8;6;2] Me ] ])}
        })
]

let targetSelectionGameStates =
    let baseGameState = {
            turnPhase = MyCardSelection 4
            myHand = (snd screenshotGameStates.[0]).myHand
            opponentsHand = [| card [5;4;5;7] Op; card [4;7;6;2] Op; card [1;7;6;4] Op; card [7;8;7;2] Op; card [1;5;3;3] Op |]
            playGrid = { slots = (array2D [ [ emptySlot; emptySlot; emptySlot ]
                                            [ emptySlot; emptySlot; emptySlot ]
                                            [ emptySlot; emptySlot; emptySlot ] ])}
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