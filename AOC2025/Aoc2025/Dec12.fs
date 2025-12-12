module Dec12

open System.IO

let filename = MetaUtils.getTodayInput 12

let inputStrings =
    File.ReadAllLines filename

let parseReq (lines:string array) =
    [|
        for i in [30..lines.Length-1] do
            let arr = lines[i].Split [|' '|]
            let areaArr = (arr[0][0..4]).Split [|'x'|]
            let area = int areaArr[0], int areaArr[1]
            let reqArr = arr[1..] |> Array.map int
            yield area,reqArr
    |]    

let calc1 lines =
    lines
    |> parseReq
    |> Array.map (fun ((x,y),arr)->
        let reqArea =
            arr
            |> Array.sum
            |> (*) 9
        let availableArea = x*y
        if availableArea >= reqArea then
            1
        else
            0          
        )
    |> Array.sum
    

let Calc() =
    inputStrings
    |> Seq.iter(printfn "%A")

    inputStrings
    |> calc1
    |> printfn "Part1: %A"
