module AI

open DomainTypes

let isTerminalNode (node: GameState) =
    node.myHand.[4].IsNone || node.opHand.[4].IsNone

let private countHandCards (hand: Hand) =
    let firstFullIndex = (hand |> Array.tryFindIndex Option.isSome)
    if firstFullIndex.IsSome then 5 - firstFullIndex.Value else 0

let evaluateNode (node: GameState) =
    let gridBalance = node.playGrid.slots |> Array.sumBy (fun slot -> 
        match slot with
            | Full c -> if c.owner = Me then 1 else -1
            | Empty _ -> 0)
    gridBalance + (countHandCards node.myHand) - (countHandCards node.opHand)


let private handWithout handIndex hand =
    let newHand = Array.copy hand
    Array.blit hand 0 newHand 1 handIndex
    newHand.[0] <- None
    newHand

let private playGridWithNewCard (playGrid: PlayGrid) (playGridIndex: int) (newCard: Card) =
    let newGridSlots = Array.copy playGrid.slots

    let updateTargetSlot () =
        let targetSlot = playGrid.slots.[playGridIndex]
        let updatedCard = if targetSlot.element.IsNone
                             then newCard
                             else let modifier = if targetSlot.element = newCard.element then +1 else -1
                                  { newCard with powerModifier = modifier}
        newGridSlots.[playGridIndex] <- Full updatedCard
                                        
    let updateNeighbor dirOffset thisPowerIndex =
        let neighborIndex = playGridIndex + dirOffset
        if neighborIndex >= 0 && neighborIndex <= 8 then
            let neighborSlot = playGrid.slots.[neighborIndex]
            let otherPowerIndex = 3 - thisPowerIndex
            if neighborSlot.isFull && neighborSlot.card.owner <> newCard.owner then
              if neighborSlot.card.modifiedPower otherPowerIndex < newCard.modifiedPower thisPowerIndex then
               newGridSlots.[neighborIndex] <- Full { neighborSlot.card with owner = newCard.owner }

    updateTargetSlot ()
    updateNeighbor -3 0 // top
    updateNeighbor -1 1 // left
    updateNeighbor +1 2 // right
    updateNeighbor +3 3 // bottom
    { slots = newGridSlots}

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
        sourceHand.[handIndex].IsSome && node.playGrid.slots.[playGridIndex].isEmpty

    let mutable validMoves = []
    for handIndex in 4 .. -1 .. 0 do
        for playGridIndex in 8 .. -1 .. 0 do
            if isValidMove handIndex playGridIndex then
                validMoves <- (handIndex, playGridIndex) :: validMoves
    validMoves |> List.map (fun move ->  move,(executeMove node move))

// function alphabeta(node, depth, α, β, maximizingPlayer)
//      if depth = 0 or node is a terminal node
//          return the heuristic value of node
//      if maximizingPlayer
//          v := -∞
//          for each child of node
//              v := max(v, alphabeta(child, depth - 1, α, β, FALSE))
//              α := max(α, v)
//              if β ≤ α
//                  break (* β cut-off *)
//          return v
//      else
//          v := ∞
//          for each child of node
//              v := min(v, alphabeta(child, depth - 1, α, β, TRUE))
//              β := min(β, v)
//              if β ≤ α
//                  break (* α cut-off *)
//          return v

let rec private alphaBeta node depth alpha beta: (int*int)*int =
    if depth = 0 || isTerminalNode node then
        (-1, -1), evaluateNode node
    elif node.turnPhase <> OpponentsTurn then
        let rec loopChildren children v bestMove alpha2 =
            match children with
                | _ when beta <= alpha2 -> (bestMove,v)
                | [] -> (bestMove,v)
                | x::xs ->
                    let newV = snd <| alphaBeta (snd x) (depth - 1) alpha2 beta
                    if newV > v 
                    then loopChildren xs newV (fst x) (max alpha2 newV)
                    else loopChildren xs v bestMove (max alpha2 newV)
        loopChildren (childStates node) System.Int32.MinValue (-1,-1) alpha
    else
        let rec loopChildren children v bestMove beta2 =
            match children with
                | _ when beta2 <= alpha -> (bestMove,v)
                | [] -> (bestMove,v)
                | x::xs ->
                    let newV = snd <| alphaBeta (snd x) (depth - 1) alpha beta2
                    if newV < v 
                    then loopChildren xs newV (fst x) (min beta2 newV)
                    else loopChildren xs v bestMove (min beta2 newV)
        loopChildren (childStates node) System.Int32.MaxValue (-1,-1) beta

        //let mutable i = 0
        //let mutable v = System.Int32.MaxValue
        //let mutable bestMove = (-1, -1)
        //let mutable beta2 = beta
        //while i < children.Length && beta2 > alpha do
        //    let (_, newV) = alphaBeta (snd children.[i]) (depth - 1) alpha beta2
        //    if newV < v then
        //        v <- newV
        //        bestMove <- fst children.[i]
        //    beta2 <- min beta2 v
        //    i <- i + 1
        //(bestMove, v)
       
let getBestMove node depth =
    alphaBeta node depth System.Int32.MinValue System.Int32.MaxValue