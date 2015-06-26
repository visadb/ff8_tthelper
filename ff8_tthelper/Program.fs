open GameStateDetection

open DomainTypes
open System.Drawing

[<EntryPoint>]
let main argv = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    //Bootstrap.savePowerModifiersFromExampleScreenshots()
    //Bootstrap.saveCursorFromExampleScreenshot()
    //Bootstrap.saveDigitFilesFromExampleScreenshot()
    //Bootstrap.digitNames <- [ "1_1"; "1_2"; "1_3"; "2_1"; "2_2"; "2_3"; "2_4"; "3_1"; "3_2"; "3_3"; "4_1"; "4_2"; "4_3"; "4_4"; "5_1"; "5_2"; "5_3"; "5_4"; "5_5"; "6_1"; "6_2"; "6_3"; "6_4"; "6_5"; "6_6"; "7_1"; "7_2"; "7_3"; "7_4"; "7_5"; "7_6"; "8_1"; "8_2"; "8_3"; "8_3"; "9_1"; "9_2"; "9_3"; "9_4"; "9_5" ]
    //Bootstrap.printDiffs()

    let screenshot = new Bitmap(screenshotDir + @"in-game\elemental_-1_in_0_1.jpg")
    printfn "%O" <| readGameState screenshot
    screenshot.Dispose()

    sw.Stop()
    printfn "Time elapsed: %A" sw.Elapsed

    0
