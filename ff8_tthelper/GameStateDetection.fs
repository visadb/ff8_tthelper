module GameStateDetection

open System.Drawing

open DomainTypes

let imageDir = System.IO.Directory.GetCurrentDirectory() + @"\..\..\images\"

let myHandPosition = Point(1381, 93)
let opponentHandPosition = Point(331, 94)
let opponentHandCardOffsets = [| 0 ; 154 ; 308; 462 ; 616 |] |> Array.map (fun y -> Size(0, y))
let myHandCardOffsets = [| 0 ; 154 ; 309; 463 ; 617 |] |> Array.map (fun y -> Size(0, y))
let cardSelectionOffset = Size(-45, 0)
let (fieldCardXOffset, fieldCardYOffset) = (0, 0)

let (digitWidth, digitHeight) = (26, 36)
let topDigitOffset = Size(15, 0)
let leftDigitOffset = Size(0, 39)
let rightDigitOffset = Size(30, 39)
let bottomDigitOffset = Size(15, 78)
let cardPowerOffsets = [| topDigitOffset ; leftDigitOffset ; rightDigitOffset ; bottomDigitOffset |]

let opponentHandCardPositions = opponentHandCardOffsets |> Array.map ((+) opponentHandPosition)
let myHandCardPositions = myHandCardOffsets |> Array.map ((+) myHandPosition)
let playGridCardPositions = array2D [ for i in 0..2 -> [ for j in 0..2 -> Point(616 + fieldCardXOffset*i, 93 + fieldCardYOffset*j) ] ]

let mutable digitNames: list<string> = []

let isDigitPixel(color: Color, relDistFromEdge: float32) =
    color.GetBrightness() > (0.5f + ((1.00f-0.5f)*(1.0f-relDistFromEdge**0.50f))) && color.GetSaturation() < 0.1f

let getDigitBitmap (screenshot: Bitmap) (point: Point): Bitmap =
    let maxAbsDistFromEdge = (float32)(min digitWidth digitHeight) / 2.0f
    let relDistanceFromEdge x y = (float32)(List.min [ x+1 ; y+1 ; digitWidth-x ; digitHeight-y ]) / maxAbsDistFromEdge

    let subImage = new Bitmap(digitWidth, digitHeight, screenshot.PixelFormat)
    seq { for y in 0 .. digitHeight-1 do
            for x in 0 .. digitWidth-1 -> (x, y, screenshot.GetPixel(point.X + x, point.Y + y)) }
        |> Seq.filter (fun (x, y, color) -> isDigitPixel(color, relDistanceFromEdge x y))
        |> Seq.iter subImage.SetPixel
    subImage

let pixelAbsDiff(pixel1: Color, pixel2: Color): int =
    abs((int)pixel2.R - (int)pixel1.R) + abs((int)pixel2.G - (int)pixel1.G) + abs((int)pixel2.B - (int)pixel2.B)

let bitmapDifference(bitmap1: Bitmap, bitmap2: Bitmap): float =
    let maxAbsDifference = bitmap1.Width * bitmap1.Height * 255 * 3
    let pixelCoords = seq { for y in 0..(bitmap1.Height-1) do
                                for x in 0..(bitmap1.Width-1) do
                                    yield (x,y) }
    let absDifference = pixelCoords
                            |> Seq.map (fun (x,y) -> (bitmap1.GetPixel(x,y), bitmap2.GetPixel(x,y)))
                            |> Seq.sumBy pixelAbsDiff

    (float)absDifference / (float)maxAbsDifference

let getModelDigitBitmapFromDisk(digit: int): Bitmap =
    new Bitmap(imageDir + "digit" + digit.ToString() + "_1.png")

let modelDigits: Bitmap list = [ for i in 1..9 -> getModelDigitBitmapFromDisk(i) ]

let readDigitValue digitBitmap: int option =
    let candidatesWithDiffs = modelDigits 
                                |> List.mapi (fun i modelDigit -> (i+1, bitmapDifference(digitBitmap, modelDigit)))
                                |> List.filter (snd >> ((>) 0.06))

    if List.isEmpty candidatesWithDiffs then
        Option.None
    else
        Option.Some (candidatesWithDiffs |> List.minBy snd |> fst)

let readCard screenshot (cardTopLeftCorner: Point): Card option =
    let powers = cardPowerOffsets |> Array.map (((+) cardTopLeftCorner) 
                                                >> (getDigitBitmap screenshot)
                                                >> readDigitValue)
    if Array.exists Option.isNone powers then
        Option.None
    else
        Some { powers = powers |> Array.map Option.get ; powerModifier = 0 ; element = None }

let readHand screenshot (handCardBasePositions: Point[]) (selectedIndex: int option): Hand =
    let shiftCardIfSelected i (cardPos: Point) =
        match selectedIndex with
            | Some(index) when i = index -> cardPos + cardSelectionOffset
            | _ -> cardPos
    handCardBasePositions |> Array.mapi (shiftCardIfSelected) |> Array.map (readCard screenshot)

let readGameState screenshot = 
    let turnPhase = MyCardSelection 0 // TODO
    let opponentsHand = lazy readHand screenshot opponentHandCardPositions Option.None
    let myHandWithSelectedCardIndex = readHand screenshot myHandCardPositions
    let playGrid = array2D [] // TODO
    match turnPhase with
        | OpponentsTurn -> { turnPhase = turnPhase
                             opponentsHand = [| |]
                             myHand = [| |]
                             playGrid = playGrid }
        | MyCardSelection i -> { turnPhase = turnPhase
                                 opponentsHand = opponentsHand.Force()
                                 myHand = myHandWithSelectedCardIndex (Some i)
                                 playGrid = playGrid }
        | MyTargetSelection _ -> { turnPhase = turnPhase
                                   opponentsHand = opponentsHand.Force()
                                   myHand = myHandWithSelectedCardIndex Option.None
                                   playGrid = playGrid }

    
module Bootstrap =
    let saveDigitFileFromScreenshot(digitName: string, point: Point, screenshot: Bitmap) =
        let digitBitmap = getDigitBitmap screenshot point
        digitBitmap.Save(imageDir + "digit"+digitName+".png", Imaging.ImageFormat.Png)
        digitBitmap.Dispose()
        digitNames <- digitNames @ [digitName]

    let saveDigitFilesFromExampleScreenshot() =
        let screenshot = new Bitmap(imageDir + "example_screenshot.jpg")

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
        saveDigitFileFromScreenshot("8_3", myHandCardPositions.[4] + rightDigitOffset, screenshot)

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
                let diff = bitmapDifference(new Bitmap(imageDir + "digit"+n1+".png"), new Bitmap(imageDir + "digit"+n2+".png"))
                diffs <- (diff, n1.[0] = n2.[0]) :: diffs

                printfn "DIFFERENCE B/W %s & %s: %f" n1 n2 diff
            done
            printfn ""
        done

        printfn "max matching diff = %f" (diffs |> List.filter (fun (_, m) -> m) |> List.maxBy (fun (d, _) -> d) |> fst)
        printfn "min non-matching diff = %f" (diffs |> List.filter (fun (_, m) -> not m) |> List.minBy (fun (d, _) -> d) |> fst)
