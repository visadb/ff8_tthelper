open System.Drawing

let imageDir = System.IO.Directory.GetCurrentDirectory() + @"\..\..\images\"

let isWhitish(color: Color) = color.GetBrightness() > 0.45f && color.GetSaturation() < 0.1f

let getDigitFromBitmap(point: Point, img: Bitmap): Bitmap =
    let (width, height) = (26, 36)
    let subImage = new Bitmap(width, height, img.PixelFormat)
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            let pixel = img.GetPixel(point.X + x, point.Y + y)
            if isWhitish(pixel) then
                subImage.SetPixel(x, y, pixel)
        done
    done
    subImage

let saveDigitFileFromScreenshot(digitName: string, point: Point, screenshot: Bitmap) =
    let digitBitmap = getDigitFromBitmap(point, screenshot)
    digitBitmap.Save(imageDir + "digit"+digitName+".png", Imaging.ImageFormat.Png)
    digitBitmap.Dispose()

let saveDigitFilesFromExampleScreenshot() =
    let screenshot = new Bitmap(imageDir + "example_screenshot.jpg")

    saveDigitFileFromScreenshot("9", Point(1350, 93), screenshot)
    saveDigitFileFromScreenshot("9_2", Point(1335, 132), screenshot)

    screenshot.Dispose()

let pixelAbsDiff(pixel1: Color, pixel2: Color): int =
    abs((int)pixel2.R - (int)pixel1.R) +
        abs((int)pixel2.G - (int)pixel1.G) +
        abs((int)pixel2.B - (int)pixel2.B)

let bitmapDifference(bitmap1: Bitmap, bitmap2: Bitmap): float =
    let maxAbsDifference = bitmap1.Width * bitmap1.Height * 255 * 3
    let pixelCoords = seq { for y in 0..(bitmap1.Height-1) do
                                for x in 0..(bitmap1.Width-1) do
                                    yield (x,y) }
    let absDifference = pixelCoords
                            |> Seq.map (fun (x,y) -> (bitmap1.GetPixel(x,y), bitmap2.GetPixel(x,y)))
                            |> Seq.sumBy pixelAbsDiff

    (float)absDifference / (float)maxAbsDifference

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    saveDigitFilesFromExampleScreenshot()

    let nineDiff = bitmapDifference(new Bitmap(imageDir + "digit9.png"), new Bitmap(imageDir + "digit9_2.png"))
    printfn "DIFFERENCE: %f" nineDiff

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0