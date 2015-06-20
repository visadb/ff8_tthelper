module GameStateDetectionTest

open FsUnit
open NUnit.Framework
open System.Drawing

open DomainTypes
open GameStateDetection

let card powers = Some { powers = List.toArray powers ; powerModifier = 0; element = None}
let slot powers = Full (card powers).Value
let emptySlot = PlayGridSlot.Empty None


let screenshotGameStates = [
    ("example_screenshot_1.jpg", {
        turnPhase = MyCardSelection 0
        myHand = [| card [9;9;5;2]; card [5;9;1;9]; card [9;8;6;2]; card [1;7;8;7]; card [8;4;8;5] |]
        opponentsHand = [| Option.None; card [4;7;6;2]; card [2;7;3;6]; card [6;5;5;4]; card [3;6;7;3] |]
        playGrid = { slots = (array2D [ [ slot [1;7;6;4]; emptySlot; emptySlot ]
                                        [ emptySlot;      emptySlot; emptySlot ]
                                        [ emptySlot;      emptySlot; emptySlot ] ])}
        })
    ("example_screenshot_2.jpg", {
        turnPhase = MyCardSelection 4
        myHand = [| Option.None; Option.None; Option.None; Option.None; card [9;9;5;2] |]
        opponentsHand = [| Option.None; Option.None; Option.None; Option.None; card [1;5;3;3] |]
        playGrid = { slots = (array2D [ [ slot [5;4;5;7]; slot [1;7;8;7]; slot [1;7;6;4] ]
                                        [ slot [7;8;7;2]; slot [4;7;6;2]; slot [5;9;1;9] ]
                                        [ slot [8;4;8;5]; emptySlot;      slot [9;8;6;2] ] ])}
        })
]

[<TestFixture>]
type ``Game state detector test`` ()=
    
    [<Test>] member x.
     ``Game state is read correctly from example screenshots`` ()=
        screenshotGameStates |> List.iter (fun (file, expectedGameState) ->
            let screenshot = new Bitmap(imageDir + file)
            readGameState screenshot |> should equal expectedGameState
            screenshot.Dispose())