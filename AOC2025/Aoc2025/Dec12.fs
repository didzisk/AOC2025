module Dec12

open System.IO

let filename = MetaUtils.getTodayInput 12

let inputStrings =
    File.ReadAllLines filename

let parse = id

let calc1 lines =
    lines
    |> parse
    

let Calc() =
    inputStrings
    |> Seq.iter(printfn "%A")

    inputStrings
    |> calc1
    |> printfn "%A"
