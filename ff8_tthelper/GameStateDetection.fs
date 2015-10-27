module GameStateDetection

open System.Drawing

open Polygon
open DomainTypes
open BitmapHelpers

let imageDir = System.IO.Directory.GetCurrentDirectory() + @"\..\..\images\"
let screenshotDir = System.IO.Directory.GetCurrentDirectory() + @"\..\..\screenshots\"

let private myHandPosition = Point(1381, 93)
let private opponentHandPosition = Point(331, 94)
let private opponentHandCardOffsets = [|0; 154; 308; 462; 616|] |> Array.map (fun y -> Size(0, y))
let private myHandCardOffsets = [|0; 154; 309; 463; 617|] |> Array.map (fun y -> Size(0, y))
let private cardSelectionOffset = Size(-45, 0)
let (private fieldCardXOffsets, private fieldCardYOffsets) = ([|0; 240; 480|], [|0; 308; 617|])

let private digitSize = Size(26, 36)
let private topDigitOffset = Size(15, 0)
let private leftDigitOffset = Size(0, 39)
let private rightDigitOffset = Size(30, 39)
let private bottomDigitOffset = Size(15, 78)
let private cardPowerOffsets = [|topDigitOffset; leftDigitOffset; rightDigitOffset; bottomDigitOffset|]

let private powerModifierOffset = Size(66, 138)
let private powerModifierSize = Size(45, 20)

let private opponentHandCardPositions = opponentHandCardOffsets |> Array.map ((+) opponentHandPosition)
let private myHandCardPositions = myHandCardOffsets |> Array.map ((+) myHandPosition)
let private playGridCardPositions =
    [| for row in 0..2 do
        for col in 0..2 ->
          Point(616 + fieldCardXOffsets.[col], 93 + fieldCardYOffsets.[row]) |]

let private cursorSize = Size(67, 46)
let private cardSelectionCursorPositions =
    [| 258; 402; 546; 690; 834 |] |> Array.map (fun y -> Point(1260, y))
let private targetSelectionCursorPositions =
    array2D [ for y in [258; 546; 834] -> [ for x in [630; 870; 1110] -> Point(x,y) ] ]

let private elementSize = Size(54, 64)
let private cardElementOffset = Size(149, 10)
let private playGridSlotElementOffset = Size(76, 107)

let resultDrawRectangle = Rectangle(837, 438, 3, 205)
let resultWinRectangle = Rectangle(1324, 438, 34, 78)
let resultLoseRectangle = Rectangle(1320, 551, 47, 57)

let spoilsSelectNumberRectangle = Rectangle(823, 110, 21, 41)

let cardChoosingScreenCardSymbolRectangle (i: int) =
    Rectangle(509, 221 + int(58.5*float(i-1)), 6, 11)

let isWhitishPixel minBr maxDiff (pixel: IntPixel) =
    let r, g, b = R pixel, G pixel, B pixel
    r > minBr && g > minBr && b > minBr
 && abs(r - g) < maxDiff && abs(r - b) < maxDiff && abs(g - b) < maxDiff

let private getDigitBitmap screenshot point =
    filteredSubBitmap screenshot (Rectangle(point, digitSize)) <| isWhitishPixel 130 13

let private getCursorBitmap screenshot point =
    filteredSubBitmap screenshot (Rectangle(point, cursorSize)) <| isWhitishPixel 200 10

let private getPowerModifierBitmap screenshot (cardTopLeft: Point) =
    let rect = Rectangle(cardTopLeft + powerModifierOffset, powerModifierSize)
    filteredSubBitmap screenshot rect (isWhitishPixel 160 10)

let private getCardElementBitmap screenshot (cardTopLeft: Point) =
    let rect = Rectangle(cardTopLeft + cardElementOffset, elementSize)
    subBitmap screenshot rect

let private getPlayGridSlotElementBitmap screenshot (cardTopLeft: Point) =
    let rect = Rectangle(cardTopLeft + playGridSlotElementOffset, elementSize)
    subBitmap screenshot rect

let private playGridSlotElementMasks = [
        BitmapMask.fromPoints [0,56;25,56;25,58;27,58;27,63;0,63]
        BitmapMask.fromPoints [0,36;25,36;25,38;28,38;28,52;25,52;25,54;24,54;24,56;22,56;22,59
                               21,59;21,61;18,61;18,63;0,63]
        BitmapMask.fromPoints [0,15;27,15;28,16;28,30;25,34;25,35;20,40;19,40;19,43;3,43;3,45
                               6,48;6,63;0,63]
    ]
let private getEmptyElementlessPlayGridSlotBitmaps doMask =
    array2D [ for row in [0..2] ->
                [ for col in [0..2] do
                  let f = sprintf "%splay_grid_slot_element_empty_%d_%d.png" imageDir row col
                  let b = SimpleBitmap.fromFile(f)
                  if doMask then
                      maskBitmapReverse playGridSlotElementMasks.[row] b
                  yield b]]

let private emptyElementlessPlayGridSlotBitmaps =
    getEmptyElementlessPlayGridSlotBitmaps true
let private emptyElementlessPlayGridSlotBitmapsWithoutMasks =
    getEmptyElementlessPlayGridSlotBitmaps false

let private getPlayGridSlotElementOnlyBitmapImpl doMask emptyColor screenshot row col =
    // Get element pixels only by undoing compositing:
    // C_o = C_a*alpha_a + C_b*alpha_b*(1-alpha_a)
    // ==> C_a = (C_o - C_b*(1-alpha_a))/alpha_a
    // alpha_b = 1.0, C_o = actual screenshot color, C_b = empty screenshot color
    let actual = getPlayGridSlotElementBitmap screenshot playGridCardPositions.[row*3 + col]
    let elementless = if doMask then emptyElementlessPlayGridSlotBitmaps.[row,col]
                                else emptyElementlessPlayGridSlotBitmapsWithoutMasks.[row,col]

    if bitmapDiff actual elementless < 0.02 then
        None
    else
        let backgroundless = SimpleBitmap.createEmpty elementSize.Width elementSize.Height
        let alpha_a = 0.64;
        let decomp chan o b = max 0 <| (int)(chan o) - (int)((float)(chan b)*(1.0-alpha_a)/alpha_a)
        seq { for y in 0 .. elementSize.Height-1 do
                for x in 0 .. elementSize.Width-1 ->
                    (x, y, actual.GetPixel(x,y), elementless.GetPixel(x,y)) }
            |> Seq.iter (fun (x, y, c_o, c_b) ->
                if A c_b = 0 || (pixelDiff c_o c_b <= 15) then
                    backgroundless.SetPixel(x, y, emptyColor)
                else
                    let elementColor = ARGB(0xff, decomp R c_o c_b, decomp G c_o c_b, decomp B c_o c_b)
                    backgroundless.SetPixel(x, y, elementColor))
        Some backgroundless

let private getPlayGridSlotElementOnlyBitmap =
    getPlayGridSlotElementOnlyBitmapImpl true 0
let private getPlayGridSlotElementOnlyBitmapWithoutTransparency =
    getPlayGridSlotElementOnlyBitmapImpl true 0xff000000
let private getPlayGridSlotElementOnlyBitmapWithoutMask =
    getPlayGridSlotElementOnlyBitmapImpl false 0

let private modelCursor = SimpleBitmap.fromFile(imageDir + "cursor.png")

let private isCursorAtPoint screenshot point: bool =
    let diff = bitmapDiff modelCursor (getCursorBitmap screenshot point)
    diff < 0.10

let private readCardOwner (screenshot: SimpleBitmap) (cardPos: Point) =
    let testArea = Rectangle(cardPos.X-1, cardPos.Y+3, 200, 15)
    let meColorBounds = ARGB(255, 178, 209, 242), ARGB(255, 188, 219, 255)
    let opColorBounds = ARGB(255, 241, 176, 208), ARGB(255, 253, 187, 220)

    let isMyPixel = isPixelBetween meColorBounds >> System.Convert.ToInt32
    let isOpPixel = isPixelBetween opColorBounds >> System.Convert.ToInt32

    let incrementCoords (x,y) =
        if testArea.Contains(x, y+1) then (x, y+1)
        else (x+1, testArea.Y)

    let pixelCounts = Seq.unfold (fun ((x,y),(me,op)) ->
            let lastCoord = (testArea.X+testArea.Width-1, testArea.Y+testArea.Height-1)
            if (max me op) >= 50 || (x,y) = lastCoord then None
            else let pixel = screenshot.GetPixel(x,y)
                 let counts = (me + isMyPixel pixel, op + isOpPixel pixel)
                 (Some (counts, (incrementCoords (x,y), counts)))) ((testArea.X, testArea.Y), (0, 0))
                            |> Seq.last

    match pixelCounts with
        | (my, op) when my > op && my > 15 -> Me
        | (my, op) when my < op && op > 15 -> Op
        | _ -> raise <| GameStateDetectionError("Unable to determine card owner")

let modelPowerModifierMinus = SimpleBitmap.fromFile(imageDir + "power_modifier_minus.png")
let modelPowerModifierPlus =  SimpleBitmap.fromFile(imageDir + "power_modifier_plus.png")

let private readPowerModifier screenshot (cardTopLeft: Point) =
    let actual = getPowerModifierBitmap screenshot cardTopLeft

    if bitmapDiff actual modelPowerModifierMinus < 0.12 then -1
    elif bitmapDiff actual modelPowerModifierPlus < 0.12 then +1
    else 0

let private modelCardElements: (Element*SimpleBitmap) list =
    Element.All
        |> List.filter (fun e -> e <> Holy && e <> Water && e <> Unknown)
        |> List.map (fun e ->
            (e, SimpleBitmap.fromFile(imageDir + "element_" + ((sprintf "%A" e).ToLower()) + ".png")))

let private readCardElement screenshot (cardTopLeft: Point): Element option =
    let cardElementBm = getCardElementBitmap screenshot cardTopLeft
    let candidatesWithDiffs =
        modelCardElements
            |> List.map (fun (e, modelBitmap) -> (e, bitmapDiff cardElementBm modelBitmap))
            |> List.filter (snd >> ((>) 0.10))

    if List.isEmpty candidatesWithDiffs then
        None
    else
        Some (candidatesWithDiffs |> List.minBy snd |> fst)

let modelDigits: SimpleBitmap list =
    [ for i in 1..10 -> SimpleBitmap.fromFile(sprintf "%sdigit%s.png" imageDir (powerToDigitName i)) ]

let private readDigitValue (digitBitmap: SimpleBitmap): int option =
    let candidatesWithDiffs =
        modelDigits |> List.mapi (fun i modelDigit -> (i+1, bitmapDiff digitBitmap modelDigit))
                    //|> List.map (fun (i,diff) -> printfn "Power %d = %f" i diff; (i,diff))
                    
    if List.isEmpty <| (candidatesWithDiffs |> List.filter (snd >> ((>) 0.17))) then
        //digitBitmap.Save(imageDir + "failed_" + ((List.minBy snd candidatesWithDiffs |> fst).ToString()) + ".png")
        None
    else
        Some (candidatesWithDiffs |> List.minBy snd |> fst)

let readCard screenshot
                     (owner: Player option)
                     (powerModifier: int option)
                     (element: Element option)
                     (cardTopLeftCorner: Point): Card option =
    let powers = cardPowerOffsets |> Array.map (((+) cardTopLeftCorner) 
                                                >> (getDigitBitmap screenshot)
                                                >> readDigitValue)
    if Array.exists Option.isNone powers then
        None
    else
        let cardOwner = if owner.IsSome then owner.Value
                        else readCardOwner screenshot cardTopLeftCorner
        let cardPowerModifier = if powerModifier.IsSome then powerModifier.Value
                                else readPowerModifier screenshot cardTopLeftCorner
        let cardElement =  if element.IsSome then element
                           else readCardElement screenshot cardTopLeftCorner
        Some { powers = powers |> Array.map Option.get; powerModifier = cardPowerModifier
               element = cardElement ; owner = cardOwner }

let private readHand screenshot
                     owner
                     (handCardBasePositions: Point[])
                     (selectedIndex: int option): Hand =
    let shiftCardIfSelected i (cardPos: Point) =
        match selectedIndex with
            | Some(index) when i = index -> cardPos + cardSelectionOffset
            | _ -> cardPos
    handCardBasePositions
        |> Array.mapi (shiftCardIfSelected)
        |> Array.map (readCard screenshot (Some owner) (Some 0) None)

let modelEmptyPlayGridSlotElements =
    [Earth,2 ; Fire,4 ; Holy,2 ; Ice,3 ; Poison,4 ; Thunder,3 ; Water,3 ; Wind,4]
        |> List.collect (fun (elem,num) ->
            let elemString = (sprintf "%A" elem).ToLower()
            [for i in [1..num] -> (SimpleBitmap.fromFile(imageDir + "slot_element_" + elemString + i.ToString() + ".png") , elem)])

let private readEmptyPlayGridSlotElement screenshot row col: Element option =
    let elementBitmapOption = getPlayGridSlotElementOnlyBitmapWithoutTransparency screenshot row col
    elementBitmapOption |> Option.map (fun bitmap ->
        modelEmptyPlayGridSlotElements
            |> List.map (fun (modelBitmap, elem) -> bitmapDiff modelBitmap bitmap, elem)
            |> List.minBy fst
            |> snd)

let private readPlayGrid screenshot: PlayGrid =
    PlayGrid(playGridCardPositions
                |> Array.map (readCard screenshot None None (Some Element.Unknown))
                |> Array.mapi (fun i oc ->
                        match oc with
                            | Some(c) -> Full c
                            | None -> Empty (readEmptyPlayGridSlotElement screenshot (i/3) (i%3))))

let private swap f a b = f b a

let private readTurnPhase screenshot =
    let selectedCardIndex =
        myHandCardPositions
            |> Array.map ((swap (+)) cardSelectionOffset)
            |> Array.tryFindIndex (fun pos ->
                (readCard screenshot (Some Me) (Some 0) (Some Element.Unknown) pos).IsSome)
    let targetSelectionPosition =
        lazy ([ for row in 0..2 do
                    for col in 0..2 -> (col, row) ]
                |> List.tryFind (fun (col, row) ->
                        isCursorAtPoint screenshot targetSelectionCursorPositions.[row,col]))

    match selectedCardIndex with
        | None -> OpponentsTurn
        | Some i when isCursorAtPoint screenshot cardSelectionCursorPositions.[i] -> MyCardSelection i
        | Some i -> MyTargetSelection (i, targetSelectionPosition.Force().Value)

let readGameStateWithTurnPhase forcedTurnPhase screenshot = 
    let turnPhase = defaultArg forcedTurnPhase (readTurnPhase screenshot)
    let opHand = lazy readHand screenshot Op opponentHandCardPositions None
    let myHandWithSelectedCardIndex = readHand screenshot Me myHandCardPositions
    let playGrid = lazy readPlayGrid screenshot
    match turnPhase with
        | OpponentsTurn -> { turnPhase = turnPhase
                             opHand = Array.empty
                             myHand = Array.empty
                             playGrid = PlayGrid.Empty }
        | MyCardSelection i -> { turnPhase = turnPhase
                                 opHand = opHand.Force()
                                 myHand = myHandWithSelectedCardIndex (Some i)
                                 playGrid = playGrid.Force() }
        | MyTargetSelection (i, _) -> { turnPhase = turnPhase
                                        opHand = opHand.Force()
                                        myHand = myHandWithSelectedCardIndex (Some i)
                                        playGrid = playGrid.Force() }

let readGameState = readGameStateWithTurnPhase None

let isAtCardSelectionConfirmationNo screenshot =
    isCursorAtPoint screenshot (Point(806, 603))

let getGamePhaseDetectionBitmap (gamePhase: GamePhase) screenshot =
    let rect = match gamePhase with
                | Draw -> resultDrawRectangle
                | Won -> resultWinRectangle
                | Lost -> resultLoseRectangle
                | _ -> invalidArg "gamePhase" "invalid"
    subBitmap screenshot rect

let modelGamePhaseDetectionBitmaps =
    [(Won,  SimpleBitmap.fromFile(imageDir + @"model_result_won.png"))
     (Draw, SimpleBitmap.fromFile(imageDir + @"model_result_draw.png"))
     (Lost, SimpleBitmap.fromFile(imageDir + @"model_result_lost.png"))]

let readGamePhase screenshot: GamePhase =
    let mostLikelyInGamePhaseWithDiff =
        modelGamePhaseDetectionBitmaps
                    |> List.map (fun (gamePhase, modelBm) ->
                            let actualBm = getGamePhaseDetectionBitmap gamePhase screenshot
                            gamePhase, bitmapDiff actualBm modelBm)
                    |> List.minBy snd
    
    match mostLikelyInGamePhaseWithDiff with
        | (gamePhase, diff) when diff < 0.03 -> gamePhase
        | _ -> Ongoing

let getSpoilsSelectionNumberBitmap screenshot = subBitmap screenshot spoilsSelectNumberRectangle

let modelSpoilsSelectionNumberBitmaps =
    [(1, SimpleBitmap.fromFile(imageDir + @"model_spoils_number_1.png"))
     (2, SimpleBitmap.fromFile(imageDir + @"model_spoils_number_2.png"))
     (4, SimpleBitmap.fromFile(imageDir + @"model_spoils_number_4.png"))]

let readSpoilsSelectionNumber screenshot =
    let bm = getSpoilsSelectionNumberBitmap screenshot
    let mostLikelySpoilsSelectionNumber =
        modelSpoilsSelectionNumberBitmaps
            |> List.map (fun (number, modelBm) -> number, bitmapDiff bm modelBm)
            |> List.minBy snd
    match mostLikelySpoilsSelectionNumber with
        | (num, diff) when diff < 0.03 -> Some num
        | _ -> None

let getCardChoosingScreenCardSymbolBitmap screenshot i =
    subBitmap screenshot (cardChoosingScreenCardSymbolRectangle i)

let modelCardSymbol = SimpleBitmap.fromFile(imageDir + @"model_card_symbol.png")

let readNumberOfCardsOnCardChoosingScreen screenshot =
    seq { for i in 2..12 ->
            bitmapDiff (getCardChoosingScreenCardSymbolBitmap screenshot i) modelCardSymbol}
        |> Seq.findIndex ((<) 0.03) |> ((+) 1)

let readRules screenshot =
    Rules.none

module Bootstrap =
    let mutable digitBitmapsFromScreenshot: Map<string, SimpleBitmap> = Map.empty

    let digitMasks: BitmapMask [] =
        [| RectangleMask []
           BitmapMask.fromPoints [7,1;13,1;13,25;15,27;15,35;9,35;4,34;4,30;7,22;7,13;5,8;5,4]
           BitmapMask.fromPoints [3,0;20,0;23,4;23,9;22,13;21,15;19,17;18,17;16,19;15,19;13,20;13,24
                                  12,27;20,27;22,25;25,26;25,31;25,35;0,35;0,30;5,25
                                  6,23;9,22;9,20;11,17;13,16;12,13;12,7;8,5;5,8;0,8;0,3;0,0]
           BitmapMask.fromPoints [1,0;20,0;22,2;20,11;22,14;25,19;25,28;21,30;18,33;16,34;13,35
                                  1,35;1,29;4,26;7,28;11,29;14,27;14,22;10,19;10,13;13,6;12,4;7,4;1,3]
           PolygonMask <| Polygon([14,0;21,0;21,5;22,18;22,19;23,29;22,31;21,35;15,35;13,31;10,28;8,27
                                   3,27;2,28;0,27;0,19;2,17;3,14;5,13;8,7;10,4])
                                 .Merge(Polygon([11,11;13,12;14,16;12,19;8,19;7,18;7,16;9,15]))
           PolygonMask <| Polygon([8,0;23,0;23,3;23,4;22,8;20,10;16,10;13,11;13,12;20,13;23,17;24,20
                                   24,25;23,28;19,26;15,22;13,21;7,21;4,19;4,12;5,10;6,3])
                                 .Merge(Polygon([6,26;8,26;11,29;15,31;15,35;4,35;3,33;3,26]))
           BitmapMask.fromPoints [14,0;18,0;17,3;15,6;11,17;11,28;12,29;16,29;17,28;17,22;15,19;15,14
                                  18,12;21,12;25,15;25,30;20,35;4,35;0,30;0,19;2,12;8,3;11,1]
           BitmapMask.fromPoints [0,0;22,0;22,7;13,30;10,35;7,35;6,34;6,28;13,12;11,11;0,11]
           BitmapMask.fromPoints [4,0;9,0;9,5;11,8;15,8;16,7;16,3;15,0;20,0;22,2;23,5;23,10;22,12;22,17
                                  24,20;24,29;21,34;13,34;13,31;15,28;15,26;12,21;10,21;7,23;7,28;10,31
                                  10,34;4,34;0,30;0,22;2,18;1,14;1,4]
           BitmapMask.fromPoints [6,0;11,0;10,9;11,14;11,19;14,19;16,17;17,13;17,10;15,4;15,0;19,0;22,2
                                  25,7;25,21;23,26;20,29;12,35;5,35;5,31;9,30;10,24;13,21;10,22;4,22
                                  1,17;0,14;0,8;3,1]
           PolygonMask <| Polygon([0,2;2,0;21,0;24,5;24,34;21,35;14,35;14,28;12,25;10,28;9,35;0,35])
                                 .Merge(Polygon([11,4;14,8;14,15;12,18;11,18;10,17;10,7]))
           |]

    let saveDigitFileFromScreenshot(digitName: string, point: Point, screenshot: SimpleBitmap) =
        let digitBitmap = getDigitBitmap screenshot point |> blurBitmap
        let mask = digitMasks.[digitNameToPower digitName]
        maskBitmap mask digitBitmap
        digitBitmap.Save(imageDir + "digit"+digitName+".png", Imaging.ImageFormat.Png)
        digitBitmapsFromScreenshot <- digitBitmapsFromScreenshot.Add(digitName, digitBitmap)

    let saveDigitFilesFromExampleScreenshot() =
        let screenshot = SimpleBitmap.fromFile(screenshotDir + @"in-game\example_screenshot_1.jpg")
        let screenshot2 = SimpleBitmap.fromFile(screenshotDir + @"in-game\card_with_power_a.jpg")

        let myCard0Selected = myHandCardPositions.[0] + cardSelectionOffset

        saveDigitFileFromScreenshot("1", myHandCardPositions.[1] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2", myCard0Selected + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("3", opponentHandCardPositions.[2] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4", myHandCardPositions.[4] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5", myCard0Selected + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6", myHandCardPositions.[2] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7", myHandCardPositions.[3] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("8", myHandCardPositions.[2] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("9", myCard0Selected + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("A", myCard0Selected + bottomDigitOffset, screenshot2)

    let printDiffs() =
        let mutable diffs = []

        let digitNames = digitBitmapsFromScreenshot |> Map.toList |> List.map fst
        for modelDigit in 1 .. 10 do
            for screenshotDigitName in digitNames do
                let modelDigitName = powerToDigitName modelDigit
                let modelBitmap = SimpleBitmap.fromFile(imageDir + "digit"+modelDigitName+".png")
                let diff = bitmapDiff modelBitmap (digitBitmapsFromScreenshot.Item screenshotDigitName)
                diffs <- (diff, modelDigitName, screenshotDigitName) :: diffs
                printfn "DIFFERENCE B/W %s & %s: %f" modelDigitName screenshotDigitName diff
            done
            printfn ""
        done
        let maxMatching = diffs |> List.filter (fun (_, n1, n2) -> n1.[0] = n2.[0])
                                |> List.maxBy (fun (d, _, _) -> d)
        let minNonMatching = diffs |> List.filter (fun (_, n1, n2) -> n1.[0] <> n2.[0])
                                   |> List.minBy (fun (d, _, _) -> d)
        printfn "max matching diff = %A" maxMatching
        printfn "min non-matching diff = %A" minNonMatching 

    let saveCursorFromExampleScreenshot() =
        let screenshot = SimpleBitmap.fromFile(screenshotDir + @"in-game\example_screenshot_1.jpg") |> blurBitmap

        let cursorBitmap = getCursorBitmap screenshot cardSelectionCursorPositions.[0]
        let cursorMask = BitmapMask.fromPoints [10,8;12,6;21,8;32,3;66,3;66,19;56,19;54,22;57,25;57,35;
                                                45,40;43,38;41,45;28,45;33,41;31,36;15,35;12,32;9,37
                                                5,32;2,30;2,21;5,20;7,12;9,11]
        maskBitmap cursorMask cursorBitmap
        cursorBitmap.Save(imageDir + "cursor.png")

    let saveSelectionCursorsFromExampleScreenshot() =
        let screenshot = SimpleBitmap.fromFile(screenshotDir + @"in-game\example_screenshot_2.jpg")

        cardSelectionCursorPositions
            |> Array.iteri (fun i pos ->
                let cursorBitmap = getCursorBitmap screenshot pos
                cursorBitmap.Save(imageDir + "cursor_"+i.ToString()+".png"))

    let savePowerModifiersFromExampleScreenshots() =
        let screenshotWithPlus = SimpleBitmap.fromFile(screenshotDir + @"in-game\elemental_+1_in_0_0.jpg")
        let screenshotWithMinus = SimpleBitmap.fromFile(screenshotDir + @"in-game\elemental_-1_in_0_0.jpg")

        let plusBitmap =
            getPowerModifierBitmap screenshotWithPlus playGridCardPositions.[0*3 + 0] |> blurBitmap
        maskBitmap (RectangleMask [Rectangle(1,2,42,11); Rectangle(14,0,15,20)]) plusBitmap
        plusBitmap.Save(imageDir + "power_modifier_plus.png")

        let minusBitmap =
            getPowerModifierBitmap screenshotWithMinus playGridCardPositions.[0*3 + 0] |> blurBitmap
        maskBitmap (RectangleMask [Rectangle(5,2,39,16)]) minusBitmap
        minusBitmap.Save(imageDir + "power_modifier_minus.png")

        printfn "Power modifier bitmap difference: %f" <| bitmapDiff plusBitmap minusBitmap

    type ElementSymbolInfo = {
        element: Element
        sourceBitmap: SimpleBitmap
        cardTopLeft: Point
        mask: BitmapMask
    }

    let getMaskedBitmap (sourceBitmap: Bitmap) = ()

    let saveElementSymbolFromExampleScreenshot (symInfo: ElementSymbolInfo) =
        let elemBitmap = getCardElementBitmap symInfo.sourceBitmap symInfo.cardTopLeft
        let elemName = (sprintf "%A" <| symInfo.element).ToLower()
        maskBitmap symInfo.mask elemBitmap
        elemBitmap.Save(imageDir + "element_"+elemName+".png")

    let saveElementSymbolsFromExampleScreenshots() =
        let example3 = SimpleBitmap.fromFile(screenshotDir + @"in-game\example_screenshot_3.jpg")
        let example4 = SimpleBitmap.fromFile(screenshotDir + @"in-game\example_screenshot_4.jpg")

        let symbolInfos = [
            { element = Fire
              sourceBitmap = example3
              cardTopLeft = myHandCardPositions.[1] + cardSelectionOffset
              mask = Polygon([33,62; 15,62; 14,59;  7,53;  4,48;  4,41;  7,37;  8,30;
                              14,24; 20,12; 24, 7; 26, 7; 26,10; 30,16; 34,18; 42,16;
                              43,12; 48,12; 49,23; 53,25; 53,38; 49,39; 48,47; 33,59])
                         |> PolygonMask }
            { element = Thunder
              sourceBitmap = example3
              cardTopLeft = myHandCardPositions.[2]
              mask = Polygon([34,62; 6,62; 6,58; 14,49; 19,48; 21,44; 21,36; 19,34; 21,29; 25,29; 36,19;
                              30,14; 49,1; 52,1; 52, 5; 45,12; 45,16; 49,19; 49,23; 37,39; 37,44; 48,44])
                        |> PolygonMask }
            { element = Poison
              sourceBitmap = example3
              cardTopLeft = myHandCardPositions.[3]
              mask =
                let topLeft = Polygon([16,28; 12,28; 7,23; 7,16; 11,11; 16,11; 20,16; 20,23])
                let topRight = Polygon([38,22; 36,19; 36,15; 39,12; 41,12; 44,15; 44,19; 41,22])
                let bottom = Polygon([36,61; 23,61; 19,56; 19,40; 23,36; 36,36; 40,40; 40,56])
                PolygonMask <| topLeft.Merge(topRight).Merge(bottom)}
            { element = Wind
              sourceBitmap = example3
              cardTopLeft = myHandCardPositions.[4]
              mask = Polygon([42,62; 30,62; 27,57; 22,57; 10,41; 10,37;  7,33;  7,21; 10,17; 11,12; 14,7
                              19, 7; 22, 3; 41, 3; 48,12; 48,29; 40,35; 40,42; 38,46; 37,51; 38,57])
                        |> PolygonMask }
            { element = Earth
              sourceBitmap = example4
              cardTopLeft = myHandCardPositions.[4] + cardSelectionOffset
              mask = Polygon([36,52; 15,52;  8,43;  8,25; 11,21; 11,16;
                              24, 2; 43, 2; 51,11; 51,29; 48,34; 48,38])
                        |> PolygonMask }
            { element = Ice
              sourceBitmap = example4
              cardTopLeft = playGridCardPositions.[1*3 + 0]
              mask = Polygon([37,56; 37,61; 33,57; 26,57; 23,61; 23,56; 26,54; 26,46; 19,37; 12,37; 10,42;
                               8,42;  8,40; 11,36; 11,28;  7,24;  8,22; 10,22; 12,27; 18,27; 25,19; 25,11;
                              23, 9; 23, 4; 26, 7; 34, 7; 37, 3; 37, 9; 34,11; 34,19; 41,28; 46,28; 49,22;
                              51,22; 51,25; 47,29; 47,35; 51,40; 51,42; 49,42; 46,36; 42,36; 33,46; 33,54])
                        |> PolygonMask }
            ]
        
        symbolInfos |> Seq.iter saveElementSymbolFromExampleScreenshot

    let saveEmptyElementlessPlayGridSlotElementBitmaps() =
        let ss1 = SimpleBitmap.fromFile(screenshotDir + @"in-game\target_selection_0_0.jpg")
        let ss2 = SimpleBitmap.fromFile(screenshotDir + @"in-game\elements\elements_11.jpg")

        [ for row in [0..2] do
            for col in [0..2] do if (row,col) <> (0,0) then yield (row,col) ]
                |> List.iter (fun (row, col) ->
                    let bmap = getPlayGridSlotElementBitmap ss1 playGridCardPositions.[row*3 + col]
                    bmap.Save(sprintf "%splay_grid_slot_element_empty_%d_%d.png" imageDir row col))

        let b0_0 = getPlayGridSlotElementBitmap ss2 playGridCardPositions.[0*3 + 0]
        b0_0.Save(imageDir + "play_grid_slot_element_empty_0_0.png")

    let saveEmptyPlayGridSlotElementBitmaps() =
        let elementScreenshots =
            [ for i in [0..26] -> SimpleBitmap.fromFile(sprintf @"%sin-game\elements\elements_%02d.jpg" screenshotDir (max i 1)) ]
        let saveSlotElem ssNum row col name =
            let b = getPlayGridSlotElementOnlyBitmapWithoutMask elementScreenshots.[ssNum] row col
            if b.IsNone then
                printfn "ERROR reading %s from ss %d %d,%d" name ssNum row col
            else
                b.Value.Save(sprintf @"%s\slot_element_%s.png" imageDir name, Imaging.ImageFormat.Png)

        saveSlotElem 19 1 0 "earth1"
        saveSlotElem 20 1 0 "earth2"

        saveSlotElem 19 2 1 "fire1"
        saveSlotElem 20 2 1 "fire2"
        saveSlotElem 21 2 1 "fire3"
        saveSlotElem 22 2 1 "fire4"

        saveSlotElem 06 0 0 "holy1"
        saveSlotElem 07 0 0 "holy2"

        saveSlotElem 24 0 1 "ice1"
        saveSlotElem 25 0 1 "ice2"
        saveSlotElem 26 0 1 "ice3"

        saveSlotElem 08 2 0 "poison1"
        saveSlotElem 02 2 2 "poison2"
        saveSlotElem 04 2 2 "poison3"
        saveSlotElem 03 2 2 "poison4"

        saveSlotElem 10 1 0 "thunder1"
        saveSlotElem 16 1 0 "thunder2"
        saveSlotElem 18 1 0 "thunder3"

        saveSlotElem 13 2 2 "water1"
        saveSlotElem 14 2 2 "water2"
        saveSlotElem 15 2 2 "water3"

        saveSlotElem 12 0 2 "wind1"
        saveSlotElem 13 0 2 "wind2"
        saveSlotElem 14 0 2 "wind3"
        saveSlotElem 15 0 2 "wind4"

    let saveResultDetectionBitmaps() =
        (getGamePhaseDetectionBitmap Draw (SimpleBitmap.fromFile(screenshotDir + @"getting_out\result_draw.jpg")))
            .Save(imageDir + "model_result_draw.png")
        (getGamePhaseDetectionBitmap Won (SimpleBitmap.fromFile(screenshotDir + @"getting_out\result_win.jpg")))
            .Save(imageDir + "model_result_won.png")
        (getGamePhaseDetectionBitmap Lost (SimpleBitmap.fromFile(screenshotDir + @"getting_out\result_lost.jpg")))
            .Save(imageDir + "model_result_lost.png")

    let saveSpoilsSelectionNumberBitmaps() =
        (getSpoilsSelectionNumberBitmap (SimpleBitmap.fromFile(screenshotDir + @"getting_out\spoils_selection_cursor1_1cards.jpg")))
            .Save(imageDir + "model_spoils_number_1.png")
        (getSpoilsSelectionNumberBitmap (SimpleBitmap.fromFile(screenshotDir + @"getting_out\spoils_selection_cursor1_2cards.jpg")))
            .Save(imageDir + "model_spoils_number_2.png")
        (getSpoilsSelectionNumberBitmap (SimpleBitmap.fromFile(screenshotDir + @"getting_out\spoils_selection_cursor1_4cards.jpg")))
            .Save(imageDir + "model_spoils_number_4.png")

    let saveCardChoosingScreenCardSymbolBitmap() =
        let screenshot = SimpleBitmap.fromFile(screenshotDir + @"getting_in\card_selection_page1.jpg")
        (getCardChoosingScreenCardSymbolBitmap screenshot 1).Save(imageDir + "model_card_symbol.png")
