module GameStateDetection

open System.Drawing

open DomainTypes

let imageDir = System.IO.Directory.GetCurrentDirectory() + @"\..\..\images\"
let screenshotDir = System.IO.Directory.GetCurrentDirectory() + @"\..\..\screenshots\"

let private myHandPosition = Point(1381, 93)
let private opponentHandPosition = Point(331, 94)
let private opponentHandCardOffsets = [| 0 ; 154 ; 308; 462 ; 616 |] |> Array.map (fun y -> Size(0, y))
let private myHandCardOffsets = [| 0 ; 154 ; 309; 463 ; 617 |] |> Array.map (fun y -> Size(0, y))
let private cardSelectionOffset = Size(-45, 0)
let (private fieldCardXOffsets, private fieldCardYOffsets) = ([| 0 ; 240 ; 480 |], [| 0 ; 308 ; 617 |])

let private digitSize = Size(26, 36)
let private topDigitOffset = Size(15, 0)
let private leftDigitOffset = Size(0, 39)
let private rightDigitOffset = Size(30, 39)
let private bottomDigitOffset = Size(15, 78)
let private cardPowerOffsets = [| topDigitOffset ; leftDigitOffset ; rightDigitOffset ; bottomDigitOffset |]

let private powerModifierOffset = Size(66, 138)
let private powerModifierSize = Size(45, 20)

let private opponentHandCardPositions = opponentHandCardOffsets |> Array.map ((+) opponentHandPosition)
let private myHandCardPositions = myHandCardOffsets |> Array.map ((+) myHandPosition)
let private playGridCardPositions = array2D [ for row in 0..2 -> [ for col in 0..2 -> Point(616 + fieldCardXOffsets.[col], 93 + fieldCardYOffsets.[row]) ] ]

let private cursorSize = Size(67, 46)
let private cardSelectionCursorPositions = [| 258; 402; 546; 690; 834 |] |> Array.map (fun y -> Point(1260, y))
let private targetSelectionCursorPositions =
    array2D [ for y in [258; 546; 834] -> [ for x in [630; 870; 1110] -> Point(x,y) ] ]

let private getFilteredSubBitmap (screenshot: Bitmap) (rect: Rectangle) (pixelFilter: Color -> bool) =
    let subBitmap = new Bitmap(rect.Width, rect.Height, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
    seq { for y in 0 .. rect.Height-1 do
            for x in 0 .. rect.Width-1 -> (x, y, screenshot.GetPixel(rect.X + x, rect.Y + y)) }
        |> Seq.iter (fun (x, y, color) -> if pixelFilter color then subBitmap.SetPixel(x,y,color) else subBitmap.SetPixel(x,y,Color.Black))
    subBitmap
    
let isWhitishPixel minBr maxDiff (color: Color) =
    let r, g, b = (int)color.R, (int)color.G, (int)color.B
    r > minBr && g > minBr && b > minBr && abs(r - g) < maxDiff && abs(r - b) < maxDiff && abs(g - b) < maxDiff

let private getDigitBitmap screenshot point =
    getFilteredSubBitmap screenshot (Rectangle(point, digitSize)) <| isWhitishPixel 130 10

let private getCursorBitmap screenshot point =
    getFilteredSubBitmap screenshot (Rectangle(point, cursorSize)) <| isWhitishPixel 200 10

let private getPowerModifierBitmap screenshot (cardTopLeft: Point) =
    getFilteredSubBitmap screenshot (Rectangle(cardTopLeft + powerModifierOffset, powerModifierSize)) <| isWhitishPixel 160 10

let private bitmapDifference (bitmap1: Bitmap) (bitmap2: Bitmap): float =
    let pixelAbsDiffAndMaxDiff(pixel1: Color, pixel2: Color): int*int =
        if pixel1.A = 0uy || pixel2.A = 0uy then
            0, 0
        else
            (abs((int)pixel2.R - (int)pixel1.R) + abs((int)pixel2.G - (int)pixel1.G) + abs((int)pixel2.B - (int)pixel1.B)), 3*255

    let pixelDiffsAndMaxDiffs = seq { for y in 0..(bitmap1.Height-1) do
                                        for x in 0..(bitmap1.Width-1) do
                                            yield pixelAbsDiffAndMaxDiff(bitmap1.GetPixel(x,y),bitmap2.GetPixel(x,y)) }
    let (absDiff, maxAbsDiff) = pixelDiffsAndMaxDiffs |> Seq.reduce (fun (d1,m1) (d2,m2) -> (d1+d2, m1+m2))
    if maxAbsDiff = 0 then 0.0
    else (float)absDiff / (float)maxAbsDiff

let private modelCursor = new Bitmap(imageDir + "cursor.png")

let private isCursorAtPoint screenshot point: bool =
    let diff = bitmapDifference modelCursor (getCursorBitmap screenshot point)
    diff < 0.10

let private readCardOwner (screenshot: Bitmap) (cardPos: Point) =
    let testArea = Rectangle(cardPos.X-1, cardPos.Y+3, 200, 15)
    let meColorBounds = Color.FromArgb(178, 209, 242), Color.FromArgb(188, 219, 255)
    let opColorBounds = Color.FromArgb(241, 176, 208), Color.FromArgb(253, 187, 220)

    let isPixelBetween (bounds: Color*Color) (pixel: Color) =
        let isBetween =
                pixel.R >= (fst bounds).R && pixel.R <= (snd bounds).R
             && pixel.G >= (fst bounds).G && pixel.G <= (snd bounds).G
             && pixel.B >= (fst bounds).B && pixel.B <= (snd bounds).B
        if isBetween then 1 else 0

    let isMyPixel = isPixelBetween meColorBounds
    let isOpPixel = isPixelBetween opColorBounds

    let incrementCoords (x,y) =
        if testArea.Contains(x, y+1) then (x, y+1)
        else (x+1, testArea.Y)

    let pixelCounts = Seq.unfold (fun ((x,y),(me,op)) ->
            if (max me op) >= 50 || (x,y) = (testArea.X+testArea.Width-1, testArea.Y+testArea.Height-1) then Option.None
            else let pixel = screenshot.GetPixel(x,y)
                 let counts = (me + isMyPixel pixel, op + isOpPixel pixel)
                 (Some (counts, (incrementCoords (x,y), counts)))) ((testArea.X, testArea.Y), (0, 0))
                            |> Seq.last

    match pixelCounts with
        | (my, op) when my > op && my > 15 -> Me
        | (my, op) when my < op && op > 15 -> Op
        | _ -> raise <| GameStateDetectionError("Unable to determine card owner")

let modelPowerModifierMinus = new Bitmap(imageDir + "power_modifier_minus.png")
let modelPowerModifierPlus =  new Bitmap(imageDir + "power_modifier_plus.png")

let private readPowerModifier screenshot (cardTopLeft: Point) =
    let actual = getPowerModifierBitmap screenshot cardTopLeft

    let minusDiff = bitmapDifference actual modelPowerModifierMinus
    let plusDiff = lazy bitmapDifference actual modelPowerModifierPlus

    if minusDiff < 0.05 then -1
    else if plusDiff.Force() < 0.05 then +1
         else 0

let private readCardElement screenshot (cardTopLeft: Point) =
    None

let private modelDigits: Bitmap list =
    let getModelDigitBitmapFromDisk(digit: int): Bitmap =
        new Bitmap(imageDir + "digit" + digit.ToString() + "_1.png")
    [ for i in 1..9 -> getModelDigitBitmapFromDisk(i) ]

let private readDigitValue digitBitmap: int option =
    let candidatesWithDiffs = modelDigits 
                                |> List.mapi (fun i modelDigit -> (i+1, bitmapDifference digitBitmap modelDigit))
                                |> List.filter (snd >> ((>) 0.16))

    if List.isEmpty candidatesWithDiffs then
        Option.None
    else
        Option.Some (candidatesWithDiffs |> List.minBy snd |> fst)

let private readCard screenshot (owner: Player option) (powerModifier: int option) (element: Element option) (cardTopLeftCorner: Point): Card option =
    let powers = cardPowerOffsets |> Array.map (((+) cardTopLeftCorner) 
                                                >> (getDigitBitmap screenshot)
                                                >> readDigitValue)
    if Array.exists Option.isNone powers then
        Option.None
    else
        let cardOwner = if owner.IsSome then owner.Value
                        else readCardOwner screenshot cardTopLeftCorner
        let cardPowerModifier = if powerModifier.IsSome then powerModifier.Value
                                else readPowerModifier screenshot cardTopLeftCorner
        Some { powers = powers |> Array.map Option.get ; powerModifier = cardPowerModifier ; element = None ; owner = cardOwner }

let private readHand screenshot owner (handCardBasePositions: Point[]) (selectedIndex: int option): Hand =
    let shiftCardIfSelected i (cardPos: Point) =
        match selectedIndex with
            | Some(index) when i = index -> cardPos + cardSelectionOffset
            | _ -> cardPos
    handCardBasePositions |> Array.mapi (shiftCardIfSelected) |> Array.map (readCard screenshot (Some owner) (Some 0) Option.None)

let private readPlayGrid screenshot: PlayGrid =
    { slots =
        playGridCardPositions
            |> Array2D.map ((readCard screenshot Option.None Option.None (Some Element.None)) >> (fun oc ->
                    match oc with Some(c) -> Full c | Option.None -> Empty None)) }

let private swap f a b = f b a

let private readTurnPhase screenshot =
    let selectedCardIndex =
        myHandCardPositions
            |> Array.map ((swap (+)) cardSelectionOffset)
            |> Array.tryFindIndex (fun pos -> (readCard screenshot (Some Me) (Some 0) (Some Element.None) pos).IsSome)
    let targetSelectionPosition =
        lazy ([ for row in 0..2 do for col in 0..2 -> (col, row) ]
                |> List.tryFind (fun (col, row) -> isCursorAtPoint screenshot targetSelectionCursorPositions.[row,col]))

    match selectedCardIndex with
        | Option.None -> OpponentsTurn
        | Some i when isCursorAtPoint screenshot cardSelectionCursorPositions.[i] -> MyCardSelection i
        | Some i -> MyTargetSelection (i, targetSelectionPosition.Force().Value)

let readGameState screenshot = 
    // TODO: card element, empty slot element
    let turnPhase = readTurnPhase screenshot
    let opHand = lazy readHand screenshot Op opponentHandCardPositions Option.None
    let myHandWithSelectedCardIndex = readHand screenshot Me myHandCardPositions
    let playGrid = lazy readPlayGrid screenshot
    match turnPhase with
        | OpponentsTurn -> { turnPhase = turnPhase
                             opHand = Array.empty
                             myHand = Array.empty
                             playGrid = { slots = array2D [] } }
        | MyCardSelection i -> { turnPhase = turnPhase
                                 opHand = opHand.Force()
                                 myHand = myHandWithSelectedCardIndex (Some i)
                                 playGrid = playGrid.Force() }
        | MyTargetSelection (i, _) -> { turnPhase = turnPhase
                                        opHand = opHand.Force()
                                        myHand = myHandWithSelectedCardIndex (Some i)
                                        playGrid = playGrid.Force() }

    
module Bootstrap =
    let mutable digitNames: list<string> = []

    let digitMasks: Rectangle list array =
        [| [];
           [Rectangle(4,1,12,35)];
           [];
           [];
           [];
           [Rectangle(3,0,22,36)];
           [];
           [Rectangle(0,0,23,11); Rectangle(11,8,8,15); Rectangle(6,23,8,13)];
           [];
           [] |]

    let rectanglePoints (size: Size) =
        seq { for y in 0..size.Height-1 do for x in 0..size.Width-1 -> (x,y)}

    let saveDigitFileFromScreenshot(digitName: string, point: Point, screenshot: Bitmap) =
        let digitBitmap = getDigitBitmap screenshot point
        let masks = digitMasks.[int <| digitName.Substring(0, 1)]
        let transparent = Color.FromArgb(0, 0, 0, 0)
        rectanglePoints digitBitmap.Size |> Seq.iter (fun (x,y) ->
            if not masks.IsEmpty && not (masks |> List.exists (fun rect -> rect.Contains(x,y))) then
                digitBitmap.SetPixel(x,y, transparent)
        )
        digitBitmap.Save(imageDir + "digit"+digitName+".png", Imaging.ImageFormat.Png)
        digitBitmap.Dispose()
        digitNames <- digitNames @ [digitName]

    let saveDigitFilesFromExampleScreenshot() =
        let screenshot = new Bitmap(screenshotDir + @"in-game\example_screenshot_1.jpg")

        let myCard0Selected = myHandCardPositions.[0] + cardSelectionOffset

        saveDigitFileFromScreenshot("1_1", myHandCardPositions.[1] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("1_2", myHandCardPositions.[3] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("1_3", playGridCardPositions.[0, 0] + topDigitOffset, screenshot)

        saveDigitFileFromScreenshot("2_1", myCard0Selected + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2_2", myHandCardPositions.[2] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2_3", opponentHandCardPositions.[1] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("2_4", opponentHandCardPositions.[2] + topDigitOffset, screenshot)

        saveDigitFileFromScreenshot("3_1", opponentHandCardPositions.[2] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("3_2", opponentHandCardPositions.[4] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("3_3", opponentHandCardPositions.[4] + bottomDigitOffset, screenshot)

        saveDigitFileFromScreenshot("4_1", myHandCardPositions.[4] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4_2", playGridCardPositions.[0, 0] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4_3", opponentHandCardPositions.[1] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("4_4", opponentHandCardPositions.[3] + bottomDigitOffset, screenshot)

        saveDigitFileFromScreenshot("5_1", myCard0Selected + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_2", myHandCardPositions.[1] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_3", myHandCardPositions.[4] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_4", opponentHandCardPositions.[3] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("5_5", opponentHandCardPositions.[3] + rightDigitOffset, screenshot)

        saveDigitFileFromScreenshot("6_1", myHandCardPositions.[2] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_2", playGridCardPositions.[0, 0] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_3", opponentHandCardPositions.[1] + rightDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_4", opponentHandCardPositions.[2] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_5", opponentHandCardPositions.[3] + topDigitOffset, screenshot)
        saveDigitFileFromScreenshot("6_6", opponentHandCardPositions.[4] + leftDigitOffset, screenshot)

        saveDigitFileFromScreenshot("7_1", myHandCardPositions.[3] + leftDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_2", myHandCardPositions.[3] + bottomDigitOffset, screenshot)
        saveDigitFileFromScreenshot("7_3", playGridCardPositions.[0, 0] + leftDigitOffset, screenshot)
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

        screenshot.Dispose()

    let printDiffs() =
        let mutable diffs = []

        for i in 0 .. digitNames.Length-1 do
            for j in i+1 .. digitNames.Length-1 do
                let (n1, n2) = (List.nth digitNames i, List.nth digitNames j)
                let diff = bitmapDifference (new Bitmap(imageDir + "digit"+n1+".png")) (new Bitmap(imageDir + "digit"+n2+".png"))
                diffs <- (diff, n1.[0] = n2.[0]) :: diffs

                printfn "DIFFERENCE B/W %s & %s: %f" n1 n2 diff
            done
            printfn ""
        done

        printfn "max matching diff = %f" (diffs |> List.filter (fun (_, m) -> m) |> List.maxBy (fun (d, _) -> d) |> fst)
        printfn "min non-matching diff = %f" (diffs |> List.filter (fun (_, m) -> not m) |> List.minBy (fun (d, _) -> d) |> fst)

    let saveCursorFromExampleScreenshot() =
        let screenshot = new Bitmap(screenshotDir + @"in-game\example_screenshot_1.jpg")

        let cursorBitmap = getCursorBitmap screenshot cardSelectionCursorPositions.[0]
        cursorBitmap.Save(imageDir + "cursor.png", Imaging.ImageFormat.Png)
        cursorBitmap.Dispose()

        screenshot.Dispose()

    let saveSelectionCursorsFromExampleScreenshot() =
        let screenshot = new Bitmap(screenshotDir + @"in-game\example_screenshot_2.jpg")

        cardSelectionCursorPositions
            |> Array.iteri (fun i pos ->
                let cursorBitmap = getCursorBitmap screenshot pos
                cursorBitmap.Save(imageDir + "cursor_"+i.ToString()+".png", Imaging.ImageFormat.Png)
                cursorBitmap.Dispose())

        screenshot.Dispose()

    let savePowerModifiersFromExampleScreenshots() =
        let screenshotWithPlus = new Bitmap(screenshotDir + @"in-game\elemental_+1_in_0_0.jpg")
        let screenshotWithMinus = new Bitmap(screenshotDir + @"in-game\elemental_-1_in_0_0.jpg")

        let plusBitmap = getPowerModifierBitmap screenshotWithPlus playGridCardPositions.[0,0]
        plusBitmap.Save(imageDir + "power_modifier_plus.png", Imaging.ImageFormat.Png)

        let minusBitmap = getPowerModifierBitmap screenshotWithMinus playGridCardPositions.[0,0]
        minusBitmap.Save(imageDir + "power_modifier_minus.png", Imaging.ImageFormat.Png)

        printfn "Power modifier bitmap difference: %f" <| bitmapDifference plusBitmap minusBitmap

        plusBitmap.Dispose()
        minusBitmap.Dispose()
        screenshotWithPlus.Dispose()
        screenshotWithMinus.Dispose()