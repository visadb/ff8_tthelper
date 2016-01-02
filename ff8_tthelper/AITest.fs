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

    { comment = "same rule triggered on 2/3 neighbors, third neighbor captured normally"
      before =
        { turnPhase = MyCardSelection 4
          myHand = [|None; None; None; None; hc [2;8;5;2] Me e|]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Op  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;4] Op 0
                                 pc [ 7;8;7;8] Op +1; pc  [4;7;6;2] Op 0;   pc [5;6;1;9] Op 0
                                 pc [10;4;8;5] Op  0; emptySlot;            pc [9;4;6;2] Op 0 |]) }
      rules = Rules.only Same; move = (4,7)
      after =
        { turnPhase = OpponentsTurn
          myHand = [|None; None; None; None; None             |]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Me  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;4] Op 0
                                 pc [ 7;8;7;8] Me +1; pc  [4;7;6;2] Me 0;   pc [5;6;1;9] Op 0
                                 pc [10;4;8;5] Me  0; pce [2;8;5;2] Me 0 e; pc [9;4;6;2] Me 0 |]) }
    }

    { comment = "same rule triggered on 2/3 neighbors, third neighbor not captured"
      before =
        { turnPhase = MyCardSelection 4
          myHand = [|None; None; None; None; hc [2;8;5;2] Me e|]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Op  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;4] Op 0
                                 pc [ 7;8;7;8] Op +1; pc  [4;7;6;2] Op 0;   pc [5;6;1;9] Op 0
                                 pc [10;4;8;5] Op  0; emptySlot;            pc [9;6;6;2] Op 0 |]) }
      rules = Rules.only Same; move = (4,7)
      after =
        { turnPhase = OpponentsTurn
          myHand = [|None; None; None; None; None             |]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Me  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;4] Op 0
                                 pc [ 7;8;7;8] Me +1; pc  [4;7;6;2] Me 0;   pc [5;6;1;9] Op 0
                                 pc [10;4;8;5] Me  0; pce [2;8;5;2] Me 0 e; pc [9;6;6;2] Op 0 |]) }
    }

    { comment = "same rule triggered on 3/3 neighbors"
      before =
        { turnPhase = MyCardSelection 4
          myHand = [|None; None; None; None; hc [2;8;5;2] Me e|]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Op  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;6] Op 0
                                 pc [ 7;8;7;8] Op +1; pc  [4;7;6;2] Op 0;   pc [5;6;1;8] Op 0
                                 pc [10;4;8;5] Op  0; emptySlot;            pc [9;5;6;2] Op 0 |]) }
      rules = Rules.only Same; move = (4,7)
      after =
        { turnPhase = OpponentsTurn
          myHand = [|None; None; None; None; None             |]
          opHand = [|None; None; None; None; hc [1;5;3;3] Op n|]
          playGrid = PlayGrid([| pc [ 5;4;9;7] Me  0; pc  [1;7;8;3] Me 0;   pc [1;7;6;6] Op 0
                                 pc [ 7;8;7;8] Me +1; pc  [4;7;6;2] Me 0;   pc [5;6;1;8] Me 0
                                 pc [10;4;8;5] Me  0; pce [2;8;5;2] Me 0 e; pc [9;5;6;2] Me 0 |]) }
    }

    { comment = "same rule triggered on 4/4 neighbors"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [4;5;3;4] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;3;1;9] Me 0
                                 emptySlot;         pc [4;7;3;6] Me 0; emptySlot       |])}
      rules = Rules.only Same; move = (2,4)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              None;              hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Op 0; emptySlot
                                 pc [1;5;5;4] Op 0; pc [4;5;3;4] Op 0; pc [5;3;1;9] Op 0
                                 emptySlot;         pc [4;7;3;6] Op 0; emptySlot       |])}
    }

    { comment = "samewall rule triggered on 1 neighbor"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2 ] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [4;3;3;10] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;3;1;9] Me 0
                                 emptySlot;         pc [4;7;3;6] Me 0; emptySlot       |])}
      rules = Rules.having([Same; SameWall]); move = (2,8)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;                           None; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;3;1;9 ] Me 0
                                 emptySlot;         pc [4;7;3;6] Op 0; pc [4;3;3;10] Op 0 |])}
    }

    { comment = "plus rule triggered on 2 neighbors"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              hc [2;1;3;2] Op u; hc [2;7;3;6] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;4;1;9] Me 0
                                 emptySlot;         emptySlot;         emptySlot       |])}
      rules = (Rules.only Same).withRule Plus; move = (1,4)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [2;7;3;6] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Op 0; emptySlot
                                 pc [1;5;5;4] Op 0; pc [2;1;3;2] Op 0; pc [5;4;1;9] Me 0
                                 emptySlot;         emptySlot;         emptySlot         |])}
    }

    { comment = "plus rule triggered on 3 neighbors"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              hc [2;1;3;2] Op u; hc [2;7;3;6] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;3;1;9] Me 0
                                 emptySlot;         emptySlot;         emptySlot       |])}
      rules = Rules.having([Same; Plus]); move = (1,4)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [2;7;3;6] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Op 0; emptySlot
                                 pc [1;5;5;4] Op 0; pc [2;1;3;2] Op 0; pc [5;3;1;9] Op 0
                                 emptySlot;         emptySlot;         emptySlot         |])}
    }

    { comment = "plus rule triggered on 4 neighbors (same sum on all 4)"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [2;1;3;2] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;3;1;9] Me 0
                                 emptySlot;         pc [4;7;3;6] Me 0; emptySlot       |])}
      rules = Rules.having([Same; Plus]); move = (2,4)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              None;              hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Op 0; emptySlot
                                 pc [1;5;5;4] Op 0; pc [2;1;3;2] Op 0; pc [5;3;1;9] Op 0
                                 emptySlot;         pc [4;7;3;6] Op 0; emptySlot       |])}
    }

    { comment = "plus rule triggered on 4 neighbors (2 different sums)"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [2;1;3;2] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;5] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;3;1;9] Me 0
                                 emptySlot;         pc [5;7;3;6] Me 0; emptySlot       |])}
      rules = Rules.having([Same; Plus]); move = (2,4)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              None;              hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;5] Op 0; emptySlot
                                 pc [1;5;5;4] Op 0; pc [2;1;3;2] Op 0; pc [5;3;1;9] Op 0
                                 emptySlot;         pc [5;7;3;6] Op 0; emptySlot       |])}
    }

    { comment = "plus rule triggered on 2 neighbors, same triggered on 2"
      before =
        { turnPhase = OpponentsTurn
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              hc [2;1;5;5] Op u; hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Me 0; emptySlot
                                 pc [1;5;5;4] Me 0; emptySlot;         pc [5;5;1;9] Me 0
                                 emptySlot;         pc [5;7;3;6] Me 0; emptySlot       |])}
      rules = Rules.having([Same; Plus]); move = (2,4)
      after =
        { turnPhase = MyCardSelection 4
          myHand = [|None;              None;              hc [9;8;6;2] Me u; hc [1;7;8;7] Me u; hc [8;4;8;5] Me u|]
          opHand = [|None;              None;              None;              hc [6;5;5;4] Op u; hc [3;6;7;3] Op u|]
          playGrid = PlayGrid([| emptySlot;         pc [9;9;5;4] Op 0; emptySlot
                                 pc [1;5;5;4] Op 0; pc [2;1;5;5] Op 0; pc [5;5;1;9] Op 0
                                 emptySlot;         pc [5;7;3;6] Op 0; emptySlot       |])}
    }
]

let executeMoveWorksCorrectly (data: ExecuteMoveTestData): unit =
    System.Console.WriteLine("executeMoveWorksCorrectly: " + data.comment)
    let newGameState = executeMove data.before data.rules data.move
    newGameState |> should equal data.after

[<TestFixture>]
type ``AI test`` ()=

    [<Test>] member x.
     ``executeMove works correctly`` ()=
        executeMoveTestData |> List.iter executeMoveWorksCorrectly