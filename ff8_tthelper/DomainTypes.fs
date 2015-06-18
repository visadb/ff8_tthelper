module DomainTypes

type Element = Earth | Fire | Holy | Ice | Thunder | Water | Wind | None
type Card = { powers: int list ; powerModifier: int ; element: Element}
type PlayGridSlot = Full of Card | Empty of Element
type TurnPhase = MyCardSelection of int | MyTargetSelection of int*int | OpponentsTurn
type GameState = {
        turnPhase: TurnPhase
        opponentsHand: Card array
        myHand: Card array
        playGrid: PlayGridSlot[,]
    }