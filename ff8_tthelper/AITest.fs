module AITest

open FsUnit
open NUnit.Framework

open DomainTypes
open AI

open TestHelpers

type ExecuteMoveTestData =
    {
        comment: string
        before: GameState
        rules: Rules
        move: int*int
        after: GameState
    }

let executeMoveTestData = [

    { comment = "basic capture"
      before =
        { turnPhase = MyCardSelection 0
          myHand = [|hc [9;9;5;2] Me e; hc [5;9;1;9] Me e; hc [9;8;6;2] Me f; hc [1;7;8;7] Me n; hc [8;4;8;5] Me n|]
          opHand = [|None;              hc [4;7;6;2] Op n; hc [2;7;3;6] Op n; hc [6;5;5;4] Op n; hc [3;6;7;3] Op n|]
          playGrid = PlayGrid([| pc [1;7;6;4] Op 0; emptySlot;         emptySlotElem e
                                 emptySlot;         emptySlot;         emptySlot
                                 emptySlot;         emptySlot;         emptySlot       |])}
      rules = Rules.none; move = (0,1)
      after =
        { turnPhase = OpponentsTurn
          myHand = [|None;              hc [5;9;1;9] Me e; hc [9;8;6;2] Me f; hc [1;7;8;7] Me n; hc [8;4;8;5] Me n|]
          opHand = [|None;              hc [4;7;6;2] Op n; hc [2;7;3;6] Op n; hc [6;5;5;4] Op n; hc [3;6;7;3] Op n|]
          playGrid = PlayGrid([| pc [1;7;6;4] Me 0; pce [9;9;5;2] Me 0 e; emptySlotElem e
                                 emptySlot;         emptySlot;            emptySlot
                                 emptySlot;         emptySlot;            emptySlot       |])}
    }

    { comment = "same rule triggered on 2/3 neighbors"
      before =
        { turnPhase = MyCardSelection 4
          myHand = [|None; None; None; None; hc [2;8;5;2] Me e|]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Op  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;4] Op 0
                                 pc [ 7;8;7;8] Op +1; pc  [4;7;6;2] Op 0;   pc [5;6;1;9] Op 0
                                 pc [10;4;8;5] Op  0; emptySlot;            pc [9;4;6;2] Op 0 |]) }
      rules = { Rules.none with isSame = true }; move = (4,7)
      after =
        { turnPhase = OpponentsTurn
          myHand = [|None; None; None; None; None             |]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Me  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;4] Op 0
                                 pc [ 7;8;7;8] Me +1; pc  [4;7;6;2] Me 0;   pc [5;6;1;9] Op 0
                                 pc [10;4;8;5] Me  0; pce [2;8;5;2] Me 0 e; pc [9;4;6;2] Me 0 |]) }
    }
]

let executeMoveWorksCorrectly (data: ExecuteMoveTestData): unit =
    System.Console.WriteLine("executeMoveWorksCorrectly: " + data.comment)
    let newGameState = executeMove data.before data.rules data.move
    newGameState |> should equal data.after
    //newGameState.turnPhase |> should equal data.after.turnPhase
    //newGameState.myHand |> should equal data.after.myHand
    //newGameState.opHand |> should equal data.after.opHand
    //newGameState.playGrid |> should equal data.after.playGrid

[<TestFixture>]
type ``AI test`` ()=

    [<Test>] member x.
     ``executeMove works correctly`` ()=
        executeMoveTestData |> List.iter executeMoveWorksCorrectly