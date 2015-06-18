module DomainTypes

type Element = Earth | Fire | Holy | Ice | Thunder | Water | Wind | None

type Card =
    { powers: int[] ; powerModifier: int ; element: Element}
    override x.ToString() = sprintf "Card %A %d %A" x.powers x.powerModifier x.element

type PlayGridSlot = Full of Card | Empty of Element
type TurnPhase = MyCardSelection of int | MyTargetSelection of int*int | OpponentsTurn
type Hand = Card option array
let handToString h = "Hand:\n  " + (h |> Array.map (sprintf "%O") |> String.concat "\n  ")
type GameState = {
        turnPhase: TurnPhase
        opponentsHand: Hand
        myHand: Hand
        playGrid: PlayGridSlot[,]
}