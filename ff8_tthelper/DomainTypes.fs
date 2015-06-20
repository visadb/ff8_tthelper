module DomainTypes

type TurnPhase = MyCardSelection of int | MyTargetSelection of int*int | OpponentsTurn

type Element = Earth | Fire | Holy | Ice | Thunder | Water | Wind | None
type Card =
    { powers: int[] ; powerModifier: int ; element: Element}
    override x.ToString() =
        let powersString = x.powers |> Array.map string |> String.concat ","
        sprintf "Card %s %+d %A" powersString x.powerModifier x.element
type PlayGridSlot = Full of Card | Empty of Element
type Hand = Card option array
let handToString h = "Hand:\n    " + (h |> Array.map (sprintf "%O") |> String.concat "\n    ")

type GameState = 
    {
        turnPhase: TurnPhase
        myHand: Hand
        opponentsHand: Hand
        playGrid: PlayGridSlot[,]
    }

    override x.ToString() = sprintf "GameState\n  turnPhase=%A\n  myHand=%O\n  opponentsHand=%O\n  playGrid=%A" 
                                    x.turnPhase
                                    (handToString x.myHand)
                                    (handToString x.opponentsHand)
                                    x.playGrid
