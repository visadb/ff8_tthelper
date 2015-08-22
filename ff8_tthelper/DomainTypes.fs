module DomainTypes

exception GameStateDetectionError of string

type HandIndex = int
type GridCoords = int*int
type TurnPhase = MyCardSelection of HandIndex | MyTargetSelection of HandIndex*GridCoords | OpponentsTurn

type Element =
    Earth | Fire | Holy | Ice | Poison | Thunder | Water | Wind | Unknown
    static member All = [ Earth; Fire; Holy; Ice; Poison; Thunder; Water; Wind ]

type Player = Me | Op
type Card =
    { powers: int[]; powerModifier: int; owner: Player; element: Element option}
    member x.modifiedPower powerIndex = x.powers.[powerIndex] + x.powerModifier
    override x.ToString() =
        let powersString = x.powers |> Array.map string |> String.concat ","
        let elementString = if x.element.IsNone then "None" else sprintf "%A" x.element.Value
        sprintf "Card %s %+d %A %s" powersString x.powerModifier x.owner elementString

type PlayGridSlot =
    Full of Card | Empty of Element option
    member x.isEmpty = match x with Full _ -> false | _ -> true
    member x.isFull = match x with Full _ -> true | _ -> false
    member x.card = match x with
                        | Full c -> c
                        | _ -> raise (new System.NullReferenceException("not Full"))
    member x.element = match x with
                        | Empty oe -> oe
                        | _ -> raise (new System.NullReferenceException("not Full"))

let playGridSlotToString slot =
    match slot with
        | Full c -> c.ToString()
        | Empty e -> sprintf "Empty GridSlot (%A)" e

type PlayGrid =
    { slots: PlayGridSlot array }
    override g.ToString() =
        "PlayGrid:\n    " + ([ for y in 0..2 -> g.slots.[y*3 .. y*3+2] |> Array.map playGridSlotToString |> String.concat "\t" ]
                            |> String.concat "\n    ")

type Hand = Card option array


let private powerModifierChar c = match c.powerModifier with -1 -> '-' | 0 -> ' ' | 1 -> '+' | _ -> '?'
let private elementChar element =
    match element with Earth -> 'E' | Fire -> 'F' | Holy -> 'H' | Ice -> 'I' | Poison -> 'P'
                     | Thunder -> 'T' | Water -> 'A' | Wind -> 'W' | Unknown -> 'U'
let private elementOptionChar elementOption =
    match elementOption with Some(e) -> elementChar e | None -> ' '
let private cardPowerString (c: Card) =
    sprintf "%d%d%d%d" c.powers.[0] c.powers.[1] c.powers.[2] c.powers.[3]
let private playerChar player = if player = Me then 'm' else 'o'
let private gridSlotString playGridSlot =
    match playGridSlot with
        | Full c -> sprintf "%c%s%c" (playerChar c.owner) (cardPowerString c) (powerModifierChar c)
        | Empty (Some e) -> sprintf " (%c)  " (elementChar e)
        | Empty (None) -> "      "
let private handSlotString handSlot =
    match handSlot with
        | Some(c) -> sprintf "%s%c" (cardPowerString c) (elementOptionChar c.element)
        | None -> "     "
let private handSelectedChar turnPhase handIndex =
    match turnPhase with
        | MyCardSelection i when i = handIndex -> '@'
        | MyTargetSelection (i,_) when i = handIndex -> '#'
        | _ -> ' '
let private gridSlotSelectedChar turnPhase gridCoords =
    match turnPhase with
        | MyTargetSelection (_,coords) when coords = gridCoords -> '@'
        | _ -> ' '
        
type GameState = 
    {
        turnPhase: TurnPhase
        myHand: Hand
        opHand: Hand
        playGrid: PlayGrid
    }

    member private x.gameStateLine (handIndex: int) (gridRow: int option) =
        if gridRow.IsSome then
            let r = gridRow.Value
            sprintf "%s %c%s |%c%s |%c%s %c%s" (handSlotString x.opHand.[handIndex])
                                               (gridSlotSelectedChar x.turnPhase (r,0))
                                               (gridSlotString x.playGrid.slots.[r*3+0])
                                               (gridSlotSelectedChar x.turnPhase (r,1))
                                               (gridSlotString x.playGrid.slots.[r*3+1])
                                               (gridSlotSelectedChar x.turnPhase (r,2))
                                               (gridSlotString x.playGrid.slots.[r*3+2])
                                               (handSelectedChar x.turnPhase handIndex)
                                               (handSlotString x.myHand.[handIndex])
        else
            sprintf "%s  -------+--------+------- %c%s" (handSlotString x.opHand.[handIndex])
                                                        (handSelectedChar x.turnPhase handIndex)
                                                        (handSlotString x.myHand.[handIndex])

    override x.ToString() =
        "\n"
      + (x.gameStateLine 0 (Some 0)) + "\n"
      + (x.gameStateLine 1 None)     + "\n"
      + (x.gameStateLine 2 (Some 1)) + "\n"
      + (x.gameStateLine 3 None)     + "\n"
      + (x.gameStateLine 4 (Some 2)) + "\n"