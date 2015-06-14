//open System.Windows.Forms

open System.Drawing


let getRectangle(rect: Rectangle, img: Bitmap): Bitmap =
    let subImage = new Bitmap(rect.Width, rect.Height, img.PixelFormat)
    for x = 0 to rect.Width-1 do
        for y = 0 to rect.Height-1 do
            subImage.SetPixel(x, y, img.GetPixel(rect.X + x, rect.Y + y))
        done
    done
    subImage


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let imgPath = @"D:\Program Files\Steam\userdata\33243684\760\remote\39150\screenshots\2015-06-13_00001.jpg"
    let bitmap = new Bitmap(imgPath)

    let numberBitmap = getRectangle(Rectangle(1350, 93, 26, 36), bitmap)
    
    numberBitmap.Save("number.png", Imaging.ImageFormat.Png)

    numberBitmap.Dispose()
    bitmap.Dispose()

    0 // return an integer exit code