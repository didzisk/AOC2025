module Dec05

open System
open System.IO

let filename = MetaUtils.getTodayInput 5

let inputStrings =
    File.ReadAllLines filename
    
let Part1 () =
    inputStrings
    |> Seq.iter (printfn "%s")
    
    let intervals =
        seq{
            for s in inputStrings do
                    if s.Contains "-" then
                        let a = s.Split('-')
                        yield (int64 a[0], int64 a[1])
        }
    let ids =
        seq{
            for s in inputStrings do
                    if (not (s.Contains "-")) && s<>"" then
                        yield int64 s
        }
    
    let fresh =
        ids
        |> Seq.filter (fun i ->
                intervals
                |> Seq.exists (fun (a,b) -> i>a && i<b)
            )
    printfn "Part1 %d" (Seq.length fresh)

[<TailCall>]
let rec mergeIntervals (intervals:(int64*int64) array)=
    let arr =
        intervals
        |> Array.sortBy fst
    let mutable isChanged = false
    for i = 0 to arr.Length - 1 do
        for j = i+1 to arr.Length - 1 do
            if (snd arr[i]) >= (fst arr[j]) then
                isChanged <- true
                arr[i] <- (Math.Min ((fst arr[i]), (fst arr[j]))),Math.Max((snd arr[i]), (snd arr[j]))
                arr[j] <- arr[i]
    if not isChanged then
        arr
    else
        mergeIntervals (Array.distinct arr)
        
let Calc2 (lines:string array) =
    
    let intervals =
        [|
            for s in lines do
                    if s.Contains "-" then
                        let a = s.Split('-')
                        yield (int64 a[0], int64 a[1])
        |]
        
    let arr = mergeIntervals intervals
    
    let fresh =
        arr
        |> Seq.map (fun (a,b) ->
                b-a + 1L
            )
        |> Seq.sum
        
    fresh    

let Part2 () =
    [|
        // "3-5"
        // "10-14"
        // "16-20"
        // "12-18"
        "3-5"
        "5-7"
    |]
    |> Calc2
    |> printfn "Ex2 %d"
    
    inputStrings
    |> Calc2
    |> printfn "Part2 %d"