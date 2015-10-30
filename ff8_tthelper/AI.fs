module AI

open DomainTypes

let inline isTerminalNode (node: GameState) =
    node.myHand.[4].IsNone || node.opHand.[4].IsNone

let inline countHandCards (hand: Hand) =
    let firstFullIndex = (hand |> Array.tryFindIndex Option.isSome)
    if firstFullIndex.IsSome then 5 - firstFullIndex.Value else 0

let inline emptyNeighbors (node: GameState) gi =
    [gi-3,0; gi-1,1; gi+1,2; gi+3,3]
        |> List.filter (fun (ngi,_) ->
            ngi >= 0 && ngi <= 8
         && ((gi%3 = ngi%3) <> (gi/3 = ngi/3)) // Either row or col changed, not both
         && node.playGrid.[ngi].isEmpty)

let inline canCardBeCaptured (node: GameState)
                      (otherPlayerMaxPowers: int array array)
                      (gridIndex: int)
                      (gridSlot: PlayGridSlot) =
    let ret = emptyNeighbors node gridIndex |> List.exists (fun (neighborIndex, powerIndex) ->
        otherPlayerMaxPowers.[neighborIndex].[3-powerIndex] > gridSlot.card.powers.[powerIndex])
    ret

let inline evaluateGridSlot (node: GameState) myHandMaxPowersInSlots opHandMaxPowersInSlots gridIndex gridSlot =
    match gridSlot with
        | Empty _ -> 0
        | Full c ->
            let otherPlayerMaxPowers = if c.owner = Me then opHandMaxPowersInSlots else myHandMaxPowersInSlots
            let canBeCaptured = canCardBeCaptured node otherPlayerMaxPowers gridIndex gridSlot
            (if c.owner = Me then 1 else -1) * (if canBeCaptured then 5 else 15)
    
let inline cardPowersInGridSlotWithElement (card: Card) (slotElem: Element option) =
    match slotElem with
        | None -> card.powers
        | eo -> card.powers |> Array.map (if eo = slotElem then ((+) 1) else (fun i -> i-1))
let inline maxPowersInGridSlotWithElem (hand: Hand) (elem: Element option) =
    hand |> Array.fold (fun maxPs oc ->
                            match oc with
                                | None -> maxPs
                                | Some c -> cardPowersInGridSlotWithElement c elem) [|-1;-1;-1;-1|]
let inline handMaxPowersInEmptyGridSlots (node: GameState) hand =
    node.playGrid.Slots |> Array.map (fun slot ->
        match slot with
            | Full _ -> Array.empty
            | Empty e -> maxPowersInGridSlotWithElem hand e
    )

let inline evaluateNode (node: GameState) =
    let myHandMaxPowersInSlots = handMaxPowersInEmptyGridSlots node node.myHand
    let opHandMaxPowersInSlots = handMaxPowersInEmptyGridSlots node node.opHand
    let gridValue = node.playGrid.Slots |> Array.mapi (evaluateGridSlot node
                                                                        myHandMaxPowersInSlots
                                                                        opHandMaxPowersInSlots)
                                          |> Array.sum
    gridValue + (countHandCards node.myHand)*10 - (countHandCards node.opHand)*10

let inline cardBalance (node: GameState) =
    let gridBalance = node.playGrid.Slots |> Array.sumBy (function
        | Empty _ -> 0 
        | Full c -> if c.owner = Me then 1 else -1)
    gridBalance + (countHandCards node.myHand) - (countHandCards node.opHand)

let inline private handWithout newHand handIndex hand =
    if handIndex < 4 then
        Array.blit hand (handIndex+1) newHand (handIndex+1) (4 - handIndex)
    Array.blit hand 0 newHand 1 handIndex
    newHand.[0] <- None
    newHand

let inline private updatePlayGrid (newPlayGrid: PlayGrid) (playGrid: PlayGrid) rules (gi: int) (newCard: Card) =
    Array.blit playGrid.Slots 0 newPlayGrid.Slots 0 9

    let updateTargetSlot () =
        let targetSlot = playGrid.[gi]
        let updatedCard = if targetSlot.element.IsNone
                             then newCard
                             else let modifier = if targetSlot.element = newCard.element then +1 else -1
                                  { newCard with powerModifier = modifier}
        newPlayGrid.Slots.[gi] <- Full updatedCard

    let getCascadingNeighborIndexes () =
        let hasTopNeighbor    = gi >= 3 &&                       playGrid.[gi-3].isFull
        let hasLeftNeighbor   = gi <> 0 && gi <> 3 && gi <> 6 && playGrid.[gi-1].isFull
        let hasRightNeighbor  = gi <> 2 && gi <> 5 && gi <> 8 && playGrid.[gi+1].isFull
        let hasBottomNeighbor = gi <= 5 &&                       playGrid.[gi+3].isFull
        let mutable cascIndexes = []
        if rules.isSame then
            if hasTopNeighbor    && playGrid.[gi-3].card.powers.[3] = newCard.powers.[0] then cascIndexes <- (gi-3) :: cascIndexes
            if hasLeftNeighbor   && playGrid.[gi-1].card.powers.[2] = newCard.powers.[1] then cascIndexes <- (gi-1) :: cascIndexes
            if hasRightNeighbor  && playGrid.[gi+1].card.powers.[1] = newCard.powers.[2] then cascIndexes <- (gi+1) :: cascIndexes
            if hasBottomNeighbor && playGrid.[gi+3].card.powers.[0] = newCard.powers.[3] then cascIndexes <- (gi+3) :: cascIndexes
            if cascIndexes.Length = 1 then cascIndexes <- []
        if rules.isPlus then
            let p0 = if hasTopNeighbor    then playGrid.[gi-3].card.powers.[3] + newCard.powers.[0] else -1
            let p1 = if hasLeftNeighbor   then playGrid.[gi-1].card.powers.[2] + newCard.powers.[1] else -1
            let p2 = if hasRightNeighbor  then playGrid.[gi+1].card.powers.[1] + newCard.powers.[2] else -1
            let p3 = if hasBottomNeighbor then playGrid.[gi+3].card.powers.[0] + newCard.powers.[3] else -1
            if p0 >= 0 && (p0 = p1 || p0 = p2 || p0 = p3) then cascIndexes <- (gi-3) :: cascIndexes
            if p1 >= 0 && (p1 = p0 || p1 = p2 || p1 = p3) then cascIndexes <- (gi-1) :: cascIndexes
            if p2 >= 0 && (p2 = p0 || p2 = p1 || p2 = p3) then cascIndexes <- (gi+1) :: cascIndexes
            if p3 >= 0 && (p3 = p0 || p3 = p1 || p3 = p2) then cascIndexes <- (gi+3) :: cascIndexes
        cascIndexes

    let newCardOwner = newCard.owner
    let rec updateNeighbor gi neighborIndex thisPowerIndex cascade =
        let neighborSlot = newPlayGrid.[neighborIndex]
        if neighborSlot.card.owner <> newCardOwner then
            let updatedNewCard = newPlayGrid.[gi].card
            let neighborPower = neighborSlot.card.modifiedPower (3 - thisPowerIndex)
            if neighborPower < updatedNewCard.modifiedPower thisPowerIndex then
                newPlayGrid.Slots.[neighborIndex] <- neighborSlot.withOppositeCardOwner
                if cascade then
                    updateNeighbors neighborIndex true

    and updateNeighbors gi2 cascade =
        if gi2 >= 3 &&                         playGrid.[gi2-3].isFull then updateNeighbor gi2 (gi2-3+0) 0 cascade // top
        if gi2 <> 0 && gi2 <> 3 && gi2 <> 6 && playGrid.[gi2-1].isFull then updateNeighbor gi2 (gi2-0-1) 1 cascade // left
        if gi2 <> 2 && gi2 <> 5 && gi2 <> 8 && playGrid.[gi2+1].isFull then updateNeighbor gi2 (gi2-0+1) 2 cascade // right
        if gi2 <= 5 &&                         playGrid.[gi2+3].isFull then updateNeighbor gi2 (gi2+3+0) 3 cascade // bottom

    updateTargetSlot ()

    let cascadingNeighborIndexes = getCascadingNeighborIndexes ()
    if cascadingNeighborIndexes.Length >= 2 then
        for neighborIndex in cascadingNeighborIndexes do
            if newPlayGrid.Slots.[neighborIndex].card.owner <> newCardOwner then
                newPlayGrid.Slots.[neighborIndex] <- newPlayGrid.Slots.[neighborIndex].withOppositeCardOwner
                updateNeighbors neighborIndex true

    updateNeighbors gi false // normal strength based capture


let inline executeMovePrealloc newPlayGrid newHand (node: GameState) rules (handIndex,playGridIndex) =
    let isMaximizingPlayer = node.turnPhase <> OpponentsTurn
    let newTurnPhase = if isMaximizingPlayer then OpponentsTurn else MyCardSelection 4
    let newOpHand = if isMaximizingPlayer then node.opHand else handWithout newHand handIndex node.opHand
    let newMyHand = if not isMaximizingPlayer then node.myHand else handWithout newHand handIndex node.myHand
    let sourceHand = if isMaximizingPlayer then node.myHand else node.opHand
    updatePlayGrid newPlayGrid node.playGrid rules playGridIndex sourceHand.[handIndex].Value
    { turnPhase = newTurnPhase; myHand = newMyHand; opHand = newOpHand; playGrid = newPlayGrid }

let executeMove node rules move =
    executeMovePrealloc (PlayGrid(Array.create 9 (Empty None))) (Array.create 5 None) node rules move

let preallocPlayGrids = [| for i in 1 .. 5*9*9 -> PlayGrid(Array.create 9 (Empty None)) |]
let preallocHands: Hand[] = [| for i in 1 .. 5*9*9 -> Array.create 5 None |]

let inline childStates (node: GameState) rules depth =
    let isMaximizingPlayer = node.turnPhase <> OpponentsTurn
    let sourceHand = if isMaximizingPlayer then node.myHand else node.opHand
    let inline isValidMove handIndex playGridIndex =
        sourceHand.[handIndex].IsSome && node.playGrid.[playGridIndex].isEmpty

    let mutable validMoves = []
    for handIndex in 4 .. -1 .. 0 do
        for playGridIndex in 8 .. -1 .. 0 do
            if isValidMove handIndex playGridIndex then
                validMoves <- (handIndex, playGridIndex) :: validMoves
    let movesWithStates = validMoves |> List.toArray
                                     |> Array.map (fun (hi,gi) -> 
                                            let move = (hi,gi)
                                            let preallocIndex = (depth-1)*5*9 + hi*9+gi
                                            move,(executeMovePrealloc preallocPlayGrids.[preallocIndex]
                                                                      preallocHands.[preallocIndex]
                                                                      node rules move))
    if isMaximizingPlayer && sourceHand.[0].IsSome then
        movesWithStates |> Array.sortInPlaceBy (fun ((_,gi),s) ->
            if canCardBeCaptured s (handMaxPowersInEmptyGridSlots s s.opHand) gi s.playGrid.[gi]
            then 1
            else -1)
    movesWithStates

let rec private alphaBeta node (rules: Rules) depth alpha beta: (int*int)*int =
    if depth = 0 || isTerminalNode node then
        (-1, -1), cardBalance node
    elif node.turnPhase <> OpponentsTurn then
        let rec loopChildren (children: ((int*int)*GameState) []) childrenIndex v bestMove alpha2 =
            if beta <= alpha2 || childrenIndex = children.Length
            then (bestMove,v)
            else let newV = snd <| alphaBeta (snd children.[childrenIndex]) rules (depth - 1) alpha2 beta
                 if newV > v then loopChildren children (childrenIndex+1) newV (fst children.[childrenIndex]) (max alpha2 newV)
                             else loopChildren children (childrenIndex+1) v bestMove alpha2
        loopChildren (childStates node rules depth) 0 System.Int32.MinValue (-1,-1) alpha
    else
        let rec loopChildren (children: ((int*int)*GameState) []) childrenIndex v bestMove beta2 =
            if beta2 <= alpha || childrenIndex = children.Length
            then (bestMove,v)
            else let newV = snd <| alphaBeta (snd children.[childrenIndex]) rules (depth - 1) alpha beta2
                 if newV < v then loopChildren children (childrenIndex+1) newV (fst children.[childrenIndex]) (min beta2 newV)
                             else loopChildren children (childrenIndex+1) v bestMove beta2
        loopChildren (childStates node rules depth) 0 System.Int32.MaxValue (-1,-1) beta
       
let getBestMove (node: GameState) (rules: Rules) depth =
    alphaBeta node rules depth System.Int32.MinValue System.Int32.MaxValue