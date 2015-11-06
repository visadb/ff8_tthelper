module BitmapHelpers

open Polygon
open System.Drawing

type IntPixel = System.Int32

let A pixel = (pixel>>>24) &&& 0xff
let R pixel = (pixel>>>16) &&& 0xff
let G pixel = (pixel>>>8) &&& 0xff
let B pixel = pixel &&& 0xff
let ARGB(a,r,g,b): IntPixel = (a<<<24) ||| (r<<<16) ||| (g<<<8) ||| b

let private rectanglePoints (rect: Rectangle) =
    seq { for y in rect.Y..(rect.Y+rect.Height-1) do for x in rect.X..(rect.X+rect.Width-1) -> (x,y) }

let private sizePoints (size: Size) =
    rectanglePoints <| Rectangle(Point(0,0), size)


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
    member this.drawFrom(source: SimpleBitmap, sourceRect: Rectangle, targetPoint: Point) =
        rectanglePoints sourceRect
            |> Seq.iter (fun (sourceX, sourceY) ->
                let thisX = (sourceX - sourceRect.X) + targetPoint.X
                let thisY = (sourceY - sourceRect.Y) + targetPoint.Y
                this.SetPixel(thisX, thisY, source.GetPixel(sourceX, sourceY)))

    static member fromFile (path: string) =
        let bitmap = new Bitmap(path)
        let lockInfo = bitmap.LockBits(new Rectangle(0,0,bitmap.Width,bitmap.Height),
                                       System.Drawing.Imaging.ImageLockMode.ReadOnly,
                                       Imaging.PixelFormat.Format32bppArgb)
        let pixels: IntPixel[] = Array.zeroCreate <| abs(lockInfo.Stride) * bitmap.Height / 4
        System.Runtime.InteropServices.Marshal.Copy(lockInfo.Scan0, pixels, 0, pixels.Length)
        bitmap.UnlockBits(lockInfo)
        bitmap.Dispose()
        { Pixels = pixels; Width = lockInfo.Width }
        
    static member createEmpty width height =
        { Pixels = Array.zeroCreate (width*height); Width = width }

type BitmapMask =
    RectangleMask of Rectangle seq | PolygonMask of Polygon

    static member fromXYWHs(xywhs: (int*int*int*int) seq) =
        RectangleMask (xywhs |> Seq.map Rectangle)

    static member fromPoints(points: (int*int) list) = PolygonMask <| Polygon(points)
    
    member this.Contains ((x,y): int*int): bool =
        match this with
           | RectangleMask masks -> masks |> Seq.exists (fun rect -> rect.Contains(x, y))
           | PolygonMask polygon -> polygon.Contains(x, y)

let private maskBitmapImpl (reverse: bool) (mask: BitmapMask) (bitmap: SimpleBitmap) =
    sizePoints bitmap.Size |> Seq.iter (fun (x,y) ->
        if reverse = mask.Contains(x,y) then
            bitmap.SetPixel(x,y, 0x00000000)
    )

let maskBitmap = maskBitmapImpl false
let maskBitmapReverse = maskBitmapImpl true

let blurBitmap (bitmap: SimpleBitmap) =
    let blurred = SimpleBitmap.createEmpty bitmap.Width bitmap.Height
    sizePoints bitmap.Size |> Seq.iter (fun (x,y) ->
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

let isPixelBetween (bounds: IntPixel*IntPixel) (pixel: IntPixel) =
    let r, g, b = R pixel, G pixel, B pixel
    let lb, ub = bounds
    (r >= R lb) && (r <= R ub)
 && (g >= G lb) && (g <= G ub)
 && (b >= B lb) && (b <= B ub)

let pixelDiff (pixel1: IntPixel) (pixel2: IntPixel) =
    abs(R pixel2 - R pixel1) + abs(G pixel2 - G pixel1) + abs(B pixel2 - B pixel1)

let bitmapDiff (bitmap1: SimpleBitmap) (bitmap2: SimpleBitmap): float =
    let pixelAbsDiffAndMaxDiff (diffSum,maxDiffSum) pixel1 pixel2: int*int =
        if A pixel1 = 0 || A pixel2 = 0
        then diffSum, maxDiffSum
        else diffSum + abs(R pixel2 - R pixel1) + abs(G pixel2 - G pixel1) + abs(B pixel2 - B pixel1), maxDiffSum + 3*255

    let (absDiff, maxAbsDiff) = Array.fold2 pixelAbsDiffAndMaxDiff (0, 0) bitmap1.Pixels bitmap2.Pixels

    if maxAbsDiff = 0 then 0.0 else (float)absDiff / (float)maxAbsDiff

let filteredSubBitmap (screenshot: SimpleBitmap) (rect: Rectangle) (pixelFilter: IntPixel -> bool) =
    let subBitmap = SimpleBitmap.createEmpty rect.Width rect.Height
    seq { for y in 0 .. rect.Height-1 do
            for x in 0 .. rect.Width-1 -> (x, y, screenshot.GetPixel(rect.X + x, rect.Y + y)) }
        |> Seq.iter (fun (x, y, color) ->
            if pixelFilter color then subBitmap.SetPixel(x,y,color)
            else subBitmap.SetPixel(x,y,0xff000000))
    subBitmap

let subBitmap (screenshot: SimpleBitmap) (rect: Rectangle) =
    filteredSubBitmap screenshot rect (fun _ -> true)