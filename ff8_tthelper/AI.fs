module AI

open DomainTypes

let isTerminalNode (node: GameState) =
    node.myHand.[4].IsNone || node.opHand.[4].IsNone

let private countHandCards (hand: Hand) =
    let firstFullIndex = (hand |> Array.tryFindIndex Option.isSome)
    if firstFullIndex.IsSome then 5 - firstFullIndex.Value else 0

let emptyNeighbors (node: GameState) gi =
    [gi-3,0; gi-1,1; gi+1,2; gi+3,3]
        |> List.filter (fun (ngi,_) ->
            ngi >= 0 && ngi <= 8
         && ((gi%3 = ngi%3) <> (gi/3 = ngi/3)) // Either row or col changed, not both
         && node.playGrid.[ngi].isEmpty)

let canCardBeCaptured (node: GameState)
                      (otherPlayerMaxPowers: int array array)
                      (gridIndex: int)
                      (gridSlot: PlayGridSlot) =
    let ret = emptyNeighbors node gridIndex |> List.exists (fun (neighborIndex, powerIndex) ->
        otherPlayerMaxPowers.[neighborIndex].[3-powerIndex] > gridSlot.card.powers.[powerIndex])
    ret

let evaluateGridSlot (node: GameState) myHandMaxPowersInSlots opHandMaxPowersInSlots gridIndex gridSlot =
    match gridSlot with
        | Empty _ -> 0
        | Full c ->
            let otherPlayerMaxPowers = if c.owner = Me then opHandMaxPowersInSlots else myHandMaxPowersInSlots
            let canBeCaptured = canCardBeCaptured node otherPlayerMaxPowers gridIndex gridSlot
            (if c.owner = Me then 1 else -1) * (if canBeCaptured then 5 else 15)
    
let cardPowersInGridSlotWithElement (card: Card) (slotElem: Element option) =
    match slotElem with
        | None -> card.powers
        | eo -> card.powers |> Array.map (if eo = slotElem then ((+) 1) else (fun i -> i-1))
let maxPowersInGridSlotWithElem (hand: Hand) (elem: Element option) =
    hand |> Array.fold (fun maxPs oc ->
                            match oc with
                                | None -> maxPs
                                | Some c -> cardPowersInGridSlotWithElement c elem) [|-1;-1;-1;-1|]
let handMaxPowersInEmptyGridSlots (node: GameState) hand =
    node.playGrid.Slots |> Array.map (fun slot ->
        match slot with
            | Full _ -> Array.empty
            | Empty e -> maxPowersInGridSlotWithElem hand e
    )

let evaluateNode (node: GameState) =
    let myHandMaxPowersInSlots = handMaxPowersInEmptyGridSlots node node.myHand
    let opHandMaxPowersInSlots = handMaxPowersInEmptyGridSlots node node.opHand
    let gridValue = node.playGrid.Slots |> Array.mapi (evaluateGridSlot node
                                                                        myHandMaxPowersInSlots
                                                                        opHandMaxPowersInSlots)
                                          |> Array.sum
    gridValue + (countHandCards node.myHand)*10 - (countHandCards node.opHand)*10

let cardBalance (node: GameState) =
    let gridBalance = node.playGrid.Slots |> Array.sumBy (function
        | Empty _ -> 0 
        | Full c -> if c.owner = Me then 1 else -1)
    gridBalance + (countHandCards node.myHand) - (countHandCards node.opHand)

let private handWithout handIndex hand =
    let newHand = Array.copy hand
    Array.blit hand 0 newHand 1 handIndex
    newHand.[0] <- None
    newHand

let private playGridWithNewCard (playGrid: PlayGrid) (playGridIndex: int) (newCard: Card) =
    let newGridSlots = Array.copy playGrid.Slots

    let updateTargetSlot () =
        let targetSlot = playGrid.[playGridIndex]
        let updatedCard = if targetSlot.element.IsNone
                             then newCard
                             else let modifier = if targetSlot.element = newCard.element then +1 else -1
                                  { newCard with powerModifier = modifier}
        newGridSlots.[playGridIndex] <- Full updatedCard
                                        
    let updateNeighbor rowOffset colOffset thisPowerIndex =
        let neighborIndex = playGridIndex + rowOffset*3 + colOffset
                                
        if neighborIndex >= 0 && neighborIndex <= 8 && (neighborIndex%3 - playGridIndex%3 = colOffset) then
            let neighborSlot = playGrid.[neighborIndex]
            let otherPowerIndex = 3 - thisPowerIndex
            if neighborSlot.isFull && neighborSlot.card.owner <> newCard.owner then
                let updatedNewCard = newGridSlots.[playGridIndex].card
                let neighborPower = neighborSlot.card.modifiedPower otherPowerIndex
                if neighborPower < updatedNewCard.modifiedPower thisPowerIndex then
                   newGridSlots.[neighborIndex] <- Full { neighborSlot.card with owner = newCard.owner }

    updateTargetSlot ()
    updateNeighbor -1  0 0 // top
    updateNeighbor  0 -1 1 // left
    updateNeighbor  0 +1 2 // right
    updateNeighbor +1  0 3 // bottom
    PlayGrid(newGridSlots)

let executeMove (node: GameState) (handIndex,playGridIndex) =
    let isMaximizingPlayer = node.turnPhase <> OpponentsTurn
    let newTurnPhase = if isMaximizingPlayer then OpponentsTurn else MyCardSelection 4
    let newOpHand = if isMaximizingPlayer then node.opHand else handWithout handIndex node.opHand
    let newMyHand = if not isMaximizingPlayer then node.myHand else handWithout handIndex node.myHand
    let sourceHand = if isMaximizingPlayer then node.myHand else node.opHand
    let newPlayGrid = playGridWithNewCard node.playGrid playGridIndex sourceHand.[handIndex].Value
    { turnPhase = newTurnPhase; myHand = newMyHand; opHand = newOpHand; playGrid = newPlayGrid}

let childStates (node: GameState) =
    let isMaximizingPlayer = node.turnPhase <> OpponentsTurn
    let sourceHand = if isMaximizingPlayer then node.myHand else node.opHand
    let isValidMove handIndex playGridIndex =
        sourceHand.[handIndex].IsSome && node.playGrid.[playGridIndex].isEmpty

    let mutable validMoves = []
    for handIndex in 4 .. -1 .. 0 do
        for playGridIndex in 8 .. -1 .. 0 do
            if isValidMove handIndex playGridIndex then
                validMoves <- (handIndex, playGridIndex) :: validMoves
    let movesWithStates = validMoves |> List.toArray
                                     |> Array.map (fun move ->  move,(executeMove node move))
    if isMaximizingPlayer && sourceHand.[0].IsSome then
        movesWithStates |> Array.sortInPlaceBy (fun ((_,gi),s) ->
            if canCardBeCaptured s (handMaxPowersInEmptyGridSlots s s.opHand) gi s.playGrid.[gi]
            then 1
            else -1)
    movesWithStates

let rec private alphaBeta node depth alpha beta: (int*int)*int =
    if depth = 0 || isTerminalNode node then
        (-1, -1), cardBalance node
    elif node.turnPhase <> OpponentsTurn then
        let rec loopChildren (children: ((int*int)*GameState) []) childrenIndex v bestMove alpha2 =
            match children with
                | _ when beta <= alpha2 -> (bestMove,v)
                | _ when childrenIndex = children.Length -> (bestMove,v)
                | _ ->
                    let newV = snd <| alphaBeta (snd children.[childrenIndex]) (depth - 1) alpha2 beta
                    if newV > v then loopChildren children (childrenIndex+1) newV (fst children.[childrenIndex]) (max alpha2 newV)
                                else loopChildren children (childrenIndex+1) v bestMove alpha2
        loopChildren (childStates node) 0 System.Int32.MinValue (-1,-1) alpha
    else
        let rec loopChildren (children: ((int*int)*GameState) []) childrenIndex v bestMove beta2 =
            match children with
                | _ when beta2 <= alpha -> (bestMove,v)
                | _ when childrenIndex = children.Length -> (bestMove,v)
                | _ ->
                    let newV = snd <| alphaBeta (snd children.[childrenIndex]) (depth - 1) alpha beta2
                    if newV < v then loopChildren children (childrenIndex+1) newV (fst children.[childrenIndex]) (min beta2 newV)
                                else loopChildren children (childrenIndex+1) v bestMove beta2
        loopChildren (childStates node) 0 System.Int32.MaxValue (-1,-1) beta
       
let getBestMove node depth =
    alphaBeta node depth System.Int32.MinValue System.Int32.MaxValue