module DomainTypes

exception GameStateDetectionError of string

let digitNameToPower(digitName: string) =
    if digitName = "A" then 10 else int digitName

let powerToDigitName(digit: int) =
    if digit = 10 then "A" else digit.ToString()

type HandIndex = int
type GridCoords = int*int

type TurnPhase =
    | MyCardSelection of HandIndex
    | MyTargetSelection of HandIndex*GridCoords
    | OpponentsTurn

type GamePhase = Ongoing | Won | Draw | Lost

type Element =
    Earth | Fire | Holy | Ice | Poison | Thunder | Water | Wind | UnknownElement
    static member All = [ Earth; Fire; Holy; Ice; Poison; Thunder; Water; Wind ]

type Player =
    Me | Op
    member x.opposite = if x = Me then Op else Me

type Card =
    { powers: int[]; powerModifier: int; owner: Player; element: Element option}
    member x.modifiedPower powerIndex = x.powers.[powerIndex] + x.powerModifier
    member x.withOppositeOwner = { x with owner = x.owner.opposite }
    override x.ToString() =
        let powersString = x.powers |> Array.map powerToDigitName |> String.concat ","
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
                        | _ -> raise (new System.NullReferenceException("not Empty"))
    member x.withOppositeCardOwner = match x with
                                      | Full c -> Full c.withOppositeOwner
                                      | _ -> raise (new System.NullReferenceException("not Full"))


let playGridSlotToString slot =
    match slot with
        | Full c -> c.ToString()
        | Empty e -> sprintf "Empty GridSlot (%A)" e

type PlayGrid(slots: PlayGridSlot array) =
    member x.Item(i) = Array.get slots i
    member x.Item(row, col) = Array.get slots (row*3 + col)
    member x.Slots = slots

    static member Empty = PlayGrid([| for i in 1..9 -> Empty None |])

    override this.Equals(other: obj) =
        this.Slots = (other :?> PlayGrid).Slots
    override this.GetHashCode() =
        this.Slots.GetHashCode()
    override g.ToString() =
        "PlayGrid:\r\n    " + ([ for y in 0..2 -> slots.[y*3 .. y*3+2] |> Array.map playGridSlotToString |> String.concat "\t" ]
                              |> String.concat "\r\n    ")

type Hand = Card option array

let private powerModifierChar c = match c.powerModifier with -1 -> '-' | 0 -> ' ' | 1 -> '+' | _ -> '?'
let private elementChar element =
    match element with Earth -> 'E' | Fire -> 'F' | Holy -> 'H' | Ice -> 'I' | Poison -> 'P'
                     | Thunder -> 'T' | Water -> 'A' | Wind -> 'W' | UnknownElement -> 'U'
let private elementOptionChar elementOption =
    match elementOption with Some(e) -> elementChar e | None -> ' '
let private cardPowerString (c: Card) =
    c.powers |> Array.map powerToDigitName |> String.concat ""
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

type Rule =
    Elemental | Open | Same | SameWall | Plus | Random | SuddenDeath | TradeOne | TradeDiff | TradeDirect | UnknownRule
type Rules =
    {
        rules: Set<Rule>
    }

    member x.withRule rule = { rules = x.rules.Add rule }
    member x.has rule = x.rules |> Set.contains rule
    member x.isValidRuleSet =
        not (x.rules.Contains UnknownRule) && (x.rules.Contains Open) && (Set.intersect x.rules Rules.tradeRules).Count = 1
    member x.tradeRule: Rule option =
        try
            Some (Set.intersect x.rules Rules.tradeRules).MinimumElement
        with
            | :? System.ArgumentException -> None

    static member having rules = { rules = Set.ofSeq rules }
    static member none = Rules.having []
    static member only rule = Rules. having [rule]
    static member tradeRules = Set.ofList([TradeOne; TradeDiff; TradeDirect])

    override x.ToString() = sprintf "Rules %A" x.rules

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
                                               (gridSlotString x.playGrid.[r,0])
                                               (gridSlotSelectedChar x.turnPhase (r,1))
                                               (gridSlotString x.playGrid.[r,1])
                                               (gridSlotSelectedChar x.turnPhase (r,2))
                                               (gridSlotString x.playGrid.[r,2])
                                               (handSelectedChar x.turnPhase handIndex)
                                               (handSlotString x.myHand.[handIndex])
        else
            sprintf "%s  -------+--------+------- %c%s" (handSlotString x.opHand.[handIndex])
                                                        (handSelectedChar x.turnPhase handIndex)
                                                        (handSlotString x.myHand.[handIndex])

    override x.ToString() =
        "\r\n"
      + (x.gameStateLine 0 (Some 0)) + "\r\n"
      + (x.gameStateLine 1 None)     + "\r\n"
      + (x.gameStateLine 2 (Some 1)) + "\r\n"
      + (x.gameStateLine 3 None)     + "\r\n"
      + (x.gameStateLine 4 (Some 2)) + "\r\n"
