module GameStateDetection

open System.Drawing

open Polygon
open DomainTypes

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

let flat row col = row*3 + col

type IntPixel = System.Int32

let A pixel = (pixel>>>24) &&& 0xff
let R pixel = (pixel>>>16) &&& 0xff
let G pixel = (pixel>>>8) &&& 0xff
let B pixel = pixel &&& 0xff
let ARGB(a,r,g,b): IntPixel = (a<<<24) ||| (r<<<16) ||| (g<<<8) ||| b

type SimpleBitmap =
    { Pixels: IntPixel[]; Width: int }
    member x.Height = x.Pixels.Length/x.Width
    member x.Size = new Size(x.Width, x.Height)
    member x.SetPixel(X,Y,pixel) = x.Pixels.[Y*x.Width + X] <- pixel
    member x.GetPixel(X,Y) = x.Pixels.[Y*x.Width + X]
    member x.asBitmap() =
        let bitmap = new Bitmap(x.Width, x.Height, Imaging.PixelFormat.Format32bppArgb)
        let lockInfo = bitmap.LockBits(new Rectangle(0,0,bitmap.Width,bitmap.Height),
                                       System.Drawing.Imaging.ImageLockMode.WriteOnly,
                                       Imaging.PixelFormat.Format32bppArgb)
        System.Runtime.InteropServices.Marshal.Copy(x.Pixels, 0, lockInfo.Scan0, x.Width*x.Height)
        bitmap.UnlockBits(lockInfo)
        bitmap
    member x.Save(path: string, imageFormat) =
        x.asBitmap().Save(path, imageFormat)
    member x.Save(path: string) =
        x.Save(path, Imaging.ImageFormat.Png)

    static member fromFile (path: string) =
        let bitmap = new Bitmap(path)
        let lockInfo = bitmap.LockBits(new Rectangle(0,0,bitmap.Width,bitmap.Height),
                                       System.Drawing.Imaging.ImageLockMode.ReadOnly,
                                       Imaging.PixelFormat.Format32bppArgb)
        let pixels: IntPixel[] = Array.zeroCreate <| abs(lockInfo.Stride) * bitmap.Height / 4
        System.Runtime.InteropServices.Marshal.Copy(lockInfo.Scan0, pixels, 0, pixels.Length)
        bitmap.Dispose()
        { Pixels = pixels; Width = lockInfo.Width }
        
    static member createEmpty width height =
        { Pixels = Array.zeroCreate (width*height); Width = width }

let private filteredSubBitmap (screenshot: SimpleBitmap) (rect: Rectangle) (pixelFilter: IntPixel -> bool) =
    let subBitmap = SimpleBitmap.createEmpty rect.Width rect.Height
    seq { for y in 0 .. rect.Height-1 do
            for x in 0 .. rect.Width-1 -> (x, y, screenshot.GetPixel(rect.X + x, rect.Y + y)) }
        |> Seq.iter (fun (x, y, color) ->
            if pixelFilter color then subBitmap.SetPixel(x,y,color)
            else subBitmap.SetPixel(x,y,0xff000000))
    subBitmap

let private subBitmap (screenshot: SimpleBitmap) (rect: Rectangle) =
    filteredSubBitmap screenshot rect (fun _ -> true)

type BitmapMask =
    RectangleMask of Rectangle list | PolygonMask of Polygon

    member this.Contains ((x,y): int*int): bool =
        match this with
           | RectangleMask masks -> masks |> List.exists (fun rect -> rect.Contains(x, y))
           | PolygonMask polygon -> polygon.Contains(x, y)
        
let private rectanglePoints (size: Size) =
    seq { for y in 0..size.Height-1 do for x in 0..size.Width-1 -> (x,y)}


let private maskBitmapImpl (reverse: bool) (mask: BitmapMask) (bitmap: SimpleBitmap) =
    rectanglePoints bitmap.Size |> Seq.iter (fun (x,y) ->
        if reverse = mask.Contains(x,y) then
            bitmap.SetPixel(x,y, 0x00000000)
    )

let private maskBitmap = maskBitmapImpl false
let private maskBitmapReverse = maskBitmapImpl true

let private blurBitmap (bitmap: SimpleBitmap) =
    let blurred = SimpleBitmap.createEmpty bitmap.Width bitmap.Height
    rectanglePoints bitmap.Size |> Seq.iter (fun (x,y) ->
        let avgCoords = [for y2 in y-1 .. y+1 do if y2>=0 && y2<=bitmap.Height-1 then yield (x,y2)]
        let blurredPixel =
            avgCoords |> List.map bitmap.GetPixel |> List.fold (fun (sr,sg,sb,c) p ->
                                if A p = 0 then (sr,sg,sb,c)
                                else (sr + R p, sg + G p, sb + G p, c+1)) (0,0,0,0)
                      |> (fun (sr,sg,sb,c) -> if c=0 then 0
                                              else ARGB(255, sr/c, sg/c, sb/c))
        blurred.SetPixel(x, y, blurredPixel)
    )
    blurred

    
let isWhitishPixel minBr maxDiff (pixel: IntPixel) =
    let r, g, b = R pixel, G pixel, B pixel
    r > minBr && g > minBr && b > minBr
 && abs(r - g) < maxDiff && abs(r - b) < maxDiff && abs(g - b) < maxDiff

let private getDigitBitmap screenshot point =
    filteredSubBitmap screenshot (Rectangle(point, digitSize)) <| isWhitishPixel 130 10

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
        PolygonMask <| Polygon([0,56; 25,56; 25,58; 27,58; 27,63; 0,63])
        PolygonMask <| Polygon([0,36; 25,36; 25,38; 28,38; 28,52; 25,52; 25,54; 24,54; 24,56; 22,56;
                                22,59; 21,59; 21,61; 18,61; 18,63; 0,63])
        PolygonMask <| Polygon([0,15; 27,15; 28,16; 28,30; 25,34; 25,35; 20,40; 19,40; 19,43; 3,43;
                                3,45; 6,48; 6,63; 0,63])
    ]
let private getEmptyElementlessPlayGridSlotBitmaps doMask =
    array2D [ for row in [0..2] do
                yield [ for col in [0..2] do
                        let b = SimpleBitmap.fromFile(imageDir + "play_grid_slot_element_empty_"+row.ToString()+"_"+col.ToString()+".png")
                        if doMask then
                            maskBitmapReverse playGridSlotElementMasks.[row] b
                        yield b]]

let private emptyElementlessPlayGridSlotBitmaps = getEmptyElementlessPlayGridSlotBitmaps true
let private emptyElementlessPlayGridSlotBitmapsWithoutMasks = getEmptyElementlessPlayGridSlotBitmaps false

let private pixelDiff (pixel1: IntPixel) (pixel2: IntPixel) =
    abs(R pixel2 - R pixel1) + abs(G pixel2 - G pixel1) + abs(B pixel2 - B pixel1)

let private bitmapDiff (bitmap1: SimpleBitmap) (bitmap2: SimpleBitmap): float =
    let pixelAbsDiffAndMaxDiff (diffSum,maxDiffSum) pixel1 pixel2: int*int =
        if A pixel1 = 0 || A pixel2 = 0
        then diffSum, maxDiffSum
        else diffSum + abs(R pixel2 - R pixel1) + abs(G pixel2 - G pixel1) + abs(B pixel2 - B pixel1), maxDiffSum + 3*255

    let (absDiff, maxAbsDiff) = Array.fold2 pixelAbsDiffAndMaxDiff (0, 0) bitmap1.Pixels bitmap2.Pixels

    if maxAbsDiff = 0 then 0.0 else (float)absDiff / (float)maxAbsDiff

let private getPlayGridSlotElementOnlyBitmapImpl doMask emptyColor screenshot row col: SimpleBitmap option =
    // Get element pixels only by undoing compositing:
    // C_o = C_a*alpha_a + C_b*alpha_b*(1-alpha_a)
    // ==> C_a = (C_o - C_b*(1-alpha_a))/alpha_a
    // alpha_b = 1.0, C_o = actual screenshot color, C_b = empty screenshot color
    let actual = getPlayGridSlotElementBitmap screenshot playGridCardPositions.[flat row col]
    let elementless = if doMask then emptyElementlessPlayGridSlotBitmaps.[row,col]
                                else emptyElementlessPlayGridSlotBitmapsWithoutMasks.[row,col]

    if bitmapDiff actual elementless < 0.02 then
        None
    else
        let backgroundless = SimpleBitmap.createEmpty elementSize.Width elementSize.Height
        let alpha_a = 0.64;
        let decomposite o b = max 0 <| (int)o - (int)((float)b*(1.0-alpha_a)/alpha_a)
        seq { for y in 0 .. elementSize.Height-1 do
                for x in 0 .. elementSize.Width-1 -> (x, y, actual.GetPixel(x, y), elementless.GetPixel(x,y)) }
            |> Seq.iter (fun (x, y, c_o, c_b) ->
                if A c_b = 0 || (pixelDiff c_o c_b <= 15) then
                    backgroundless.SetPixel(x, y, emptyColor)
                else
                    let elementColor = ARGB(0xff, decomposite (R c_o) (R c_b), decomposite (G c_o) (G c_b), decomposite (B c_o) (B c_b))
                    backgroundless.SetPixel(x, y, elementColor))
        Some backgroundless

let private getPlayGridSlotElementOnlyBitmap = getPlayGridSlotElementOnlyBitmapImpl true 0
let private getPlayGridSlotElementOnlyBitmapWithoutTransparency = getPlayGridSlotElementOnlyBitmapImpl true 0xff000000
let private getPlayGridSlotElementOnlyBitmapWithoutMask = getPlayGridSlotElementOnlyBitmapImpl false 0

let private modelCursor = SimpleBitmap.fromFile(imageDir + "cursor.png")

let private isCursorAtPoint screenshot point: bool =
    let diff = bitmapDiff modelCursor (getCursorBitmap screenshot point)
    diff < 0.10

let private readCardOwner (screenshot: SimpleBitmap) (cardPos: Point) =
    let testArea = Rectangle(cardPos.X-1, cardPos.Y+3, 200, 15)
    let meColorBounds = ARGB(255, 178, 209, 242), ARGB(255, 188, 219, 255)
    let opColorBounds = ARGB(255, 241, 176, 208), ARGB(255, 253, 187, 220)

    let isPixelBetween (bounds: IntPixel*IntPixel) (pixel: IntPixel) =
        let isBetween =
                let r, g, b = R pixel, G pixel, B pixel
                let lBound, uBound = bounds
                (r >= R lBound) && (r <= R uBound)
             && (g >= G lBound) && (g <= G uBound)
             && (b >= B lBound) && (b <= B uBound)
        if isBetween then 1 else 0

    let isMyPixel = isPixelBetween meColorBounds
    let isOpPixel = isPixelBetween opColorBounds

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

    let minusDiff = bitmapDiff actual modelPowerModifierMinus
    let plusDiff = lazy bitmapDiff actual modelPowerModifierPlus

    if minusDiff < 0.12 then -1
    else if plusDiff.Force() < 0.12 then +1
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
    let getModelDigitBitmapFromDisk(digit: int): SimpleBitmap =
        SimpleBitmap.fromFile(imageDir + "digit" + digit.ToString() + "_1.png")
    [ for i in 1..9 -> getModelDigitBitmapFromDisk(i) ]

let private readDigitValue digitBitmap: int option =
    let candidatesWithDiffs =
        modelDigits |> List.mapi (fun i modelDigit -> (i+1, bitmapDiff digitBitmap modelDigit))
                    |> List.filter (snd >> ((>) 0.16))

    if List.isEmpty candidatesWithDiffs then
        None
    else
        Some (candidatesWithDiffs |> List.minBy snd |> fst)

let private readCard screenshot
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
    { slots =
        playGridCardPositions
            |> Array.map (readCard screenshot None None (Some Element.Unknown))
            |> Array.mapi (fun i oc ->
                    match oc with
                        | Some(c) -> Full c
                        | None -> Empty (readEmptyPlayGridSlotElement screenshot (i/3) (i%3))) }

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

let readGameState screenshot = 
    let turnPhase = readTurnPhase screenshot
    let opHand = lazy readHand screenshot Op opponentHandCardPositions None
    let myHandWithSelectedCardIndex = readHand screenshot Me myHandCardPositions
    let playGrid = lazy readPlayGrid screenshot
    match turnPhase with
        | OpponentsTurn -> { turnPhase = turnPhase
                             opHand = Array.empty
                             myHand = Array.empty
                             playGrid = { slots = [||] } }
        | MyCardSelection i -> { turnPhase = turnPhase
                                 opHand = opHand.Force()
                                 myHand = myHandWithSelectedCardIndex (Some i)
                                 playGrid = playGrid.Force() }
        | MyTargetSelection (i, _) -> { turnPhase = turnPhase
                                        opHand = opHand.Force()
                                        myHand = myHandWithSelectedCardIndex (Some i)
                                        playGrid = playGrid.Force() }

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
    [(2, SimpleBitmap.fromFile(imageDir + @"model_spoils_number_2.png"))
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


module Bootstrap =
    let mutable digitBitmapsFromScreenshot: Map<string, SimpleBitmap> = Map.empty

    let digitMasks: Rectangle list array =
        [| [];
           [(4,1,12,35)];
           [(0,0,25,9); (12,9,13,11); (0,20,15,16); (14,24,12,11)];
           [(3,0,20,5); (9,5,16,31); (0,27,9,9)];
           [(0,19,12,10); (2,14,6,5); (5,10,6,5); (7,4,5,7); (12,0,12,19); (12,19,13,17)];
           [(4,0,20,11); (3,11,11,11); (12,13,13,9); (15,21,10,7); (2,26,8,10); (10,29,7,7)];
           [(12,0,8,3); (8,3,9,3); (6,6,10,3); (3,9,11,3); (2,12,12,3); (1,15,11,4); (0,19,12,17);
            (12,30,14,6); (17,21,9,9); (15,13,11,8)];
           [(0,0,23,11); (10,8,10,15); (6,23,8,13)];
           [(1,0,23,4); (0,4,11,6); (15,4,9,6); (0,10,23,12); (0,20,9,11); (13,20,12,5); (15,25,10,6);
            (1,31,21,4)];
           [(0,0,12,23); (15,0,11,21); (11,20,14,5); (10,25,13,4); (9,29,11,3); (5,32,10,4)]
           |] |> Array.map (List.map Rectangle)

    let saveDigitFileFromScreenshot(digitName: string, point: Point, screenshot: SimpleBitmap) =
        let digitBitmap = getDigitBitmap screenshot point |> blurBitmap
        let masks = digitMasks.[int <| digitName.Substring(0, 1)]
        maskBitmap (RectangleMask masks) digitBitmap
        digitBitmap.Save(imageDir + "digit"+digitName+".png", Imaging.ImageFormat.Png)
        digitBitmapsFromScreenshot <- digitBitmapsFromScreenshot.Add(digitName, digitBitmap)

    let saveDigitFilesFromExampleScreenshot() =
        let screenshot = SimpleBitmap.fromFile(screenshotDir + @"in-game\example_screenshot_1.jpg")

        let myCard0Selected = myHandCardPositions.[0] + cardSelectionOffset

        saveDigitFileFromScreenshot("1_1", myHandCardPositions.[1] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("1_2", myHandCardPositions.[3] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("1_3", playGridCardPositions.[flat 0 0] + topDigitOffset, screenshot)

        saveDigitFileFromScreenshot("2_1", myCard0Selected + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2_2", myHandCardPositions.[2] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2_3", opponentHandCardPositions.[1] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2_4", opponentHandCardPositions.[2] + topDigitOffset, screenshot)

        saveDigitFileFromScreenshot("3_1", opponentHandCardPositions.[2] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("3_2", opponentHandCardPositions.[4] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("3_3", opponentHandCardPositions.[4] + bottomDigitOffset, screenshot)

        saveDigitFileFromScreenshot("4_1", myHandCardPositions.[4] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4_2", playGridCardPositions.[flat 0 0] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4_3", opponentHandCardPositions.[1] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4_4", opponentHandCardPositions.[3] + bottomDigitOffset, screenshot)

        saveDigitFileFromScreenshot("5_1", myCard0Selected + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_2", myHandCardPositions.[1] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_3", myHandCardPositions.[4] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_4", opponentHandCardPositions.[3] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_5", opponentHandCardPositions.[3] + rightDigitOffset, screenshot)

        saveDigitFileFromScreenshot("6_1", myHandCardPositions.[2] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_2", playGridCardPositions.[flat 0 0] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_3", opponentHandCardPositions.[1] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_4", opponentHandCardPositions.[2] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_5", opponentHandCardPositions.[3] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_6", opponentHandCardPositions.[4] + leftDigitOffset, screenshot)

        saveDigitFileFromScreenshot("7_1", myHandCardPositions.[3] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_2", myHandCardPositions.[3] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_3", playGridCardPositions.[flat 0 0] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_4", opponentHandCardPositions.[1] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_5", opponentHandCardPositions.[2] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_6", opponentHandCardPositions.[4] + rightDigitOffset, screenshot)

        saveDigitFileFromScreenshot("8_1", myHandCardPositions.[2] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("8_2", myHandCardPositions.[3] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("8_3", myHandCardPositions.[4] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("8_4", myHandCardPositions.[4] + rightDigitOffset, screenshot)

        saveDigitFileFromScreenshot("9_1", myCard0Selected + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("9_2", myCard0Selected + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("9_3", myHandCardPositions.[1] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("9_4", myHandCardPositions.[1] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("9_5", myHandCardPositions.[2] + topDigitOffset, screenshot)

    let printDiffs() =
        let mutable diffs = []

        let digitNames = digitBitmapsFromScreenshot |> Map.toList |> List.map fst
        for modelDigit in 1 .. 9 do
            for screenshotDigitName in digitNames do
                let modelDigitName = sprintf "%d_1" modelDigit
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
        let cursorMask =
            RectangleMask [Rectangle(2,6,56,34); Rectangle(31,1,36,19); Rectangle(27,28,23,18)]
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
            getPowerModifierBitmap screenshotWithPlus playGridCardPositions.[flat 0 0] |> blurBitmap
        maskBitmap (RectangleMask [Rectangle(1,2,42,11); Rectangle(14,0,15,20)]) plusBitmap
        plusBitmap.Save(imageDir + "power_modifier_plus.png")

        let minusBitmap =
            getPowerModifierBitmap screenshotWithMinus playGridCardPositions.[flat 0 0] |> blurBitmap
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
              cardTopLeft = playGridCardPositions.[flat 1 0]
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
                    let bmap = getPlayGridSlotElementBitmap ss1 playGridCardPositions.[flat row col]
                    bmap.Save(sprintf "%splay_grid_slot_element_empty_%d_%d.png" imageDir row col))

        let b0_0 = getPlayGridSlotElementBitmap ss2 playGridCardPositions.[flat 0 0]
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
        (getSpoilsSelectionNumberBitmap (SimpleBitmap.fromFile(screenshotDir + @"getting_out\spoils_selection_cursor1_2cards.jpg")))
            .Save(imageDir + "model_spoils_number_2.png")
        (getSpoilsSelectionNumberBitmap (SimpleBitmap.fromFile(screenshotDir + @"getting_out\spoils_selection_cursor1_4cards.jpg")))
            .Save(imageDir + "model_spoils_number_4.png")
