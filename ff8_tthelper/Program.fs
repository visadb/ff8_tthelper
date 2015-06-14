open System.Windows.Forms

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    SendKeys.SendWait("{PGUP}")
    0 // return an integer exit code