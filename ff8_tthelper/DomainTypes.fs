module DomainTypes

exception GameStateDetectionError of string

type HandIndex = int
type GridCoords = int*int
type TurnPhase = MyCardSelection of HandIndex | MyTargetSelection of HandIndex*GridCoords | OpponentsTurn

type Element = Earth | Fire | Holy | Ice | Thunder | Water | Wind | None
type Player = Me | Op
type Card =
    { powers: int[] ; powerModifier: int ; element: Element ; owner: Player}
    override x.ToString() =
        let powersString = x.powers |> Array.map string |> String.concat ","
        sprintf "Card %s %+d %A %A" powersString x.powerModifier x.element x.owner

type PlayGridSlot = Full of Card | Empty of Element
let playGridSlotToString slot =
    match slot with
        | Full c -> c.ToString()
        | Empty e -> sprintf "Empty GridSlot (%A)" e

type PlayGrid =
    { slots: PlayGridSlot[,] }
    override g.ToString() =
        "PlayGrid:\n    " + ([ for y in 0..2 -> g.slots.[y, *] |> Array.map playGridSlotToString |> String.concat "\t" ]
                            |> String.concat "\n    ")

type Hand = Card option array
let handToString h = "Hand:\n    " + (h |> Array.map (sprintf "%O") |> String.concat "\n    ")

type GameState = 
    {
        turnPhase: TurnPhase
        myHand: Hand
        opHand: Hand
        playGrid: PlayGrid
    }

    override x.ToString() = sprintf "GameState:\n  turnPhase = %A\n  myHand = %O\n  opHand = %O\n  playGrid = %O" 
                                    x.turnPhase
                                    (handToString x.myHand)
                                    (handToString x.opHand)
                                    x.playGrid
