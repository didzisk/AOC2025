module Dec04

open System
open System.IO

let filename = MetaUtils.getTodayInput 4

let inputStrings =
    File.ReadAllLines filename
    |> Seq.map _.ToCharArray()
    |> Array.ofSeq

let calc1 (arr:char array array) =
    [|
        for r = 0 to arr.Length-1 do
            seq{
                for c = 0 to arr[r].Length-1 do
                    seq{
                        for dr,dc in ArrayUtils.dirsX do
                            let pos = (r+dr,c+dc)
                            if ArrayUtils.validPos arr pos then
                                if arr[fst pos][snd pos] = '@' then
                                    yield 1
                    }
                    |> Seq.sum
                    |> fun x->
                           if arr[r][c]='@' then 
                               if x < 4 then
                                  'x'
                               else '@'
                           else '.'
                    }
            |> Seq.toArray
    |]
    
let printArr (arr:char array array) =
    arr |> Seq.iter (String >> printfn "%s")
    printfn ""
    
let ex = 
    File.ReadAllLines (MetaUtils.todayFilenameEx 4)
    |> Seq.map _.ToCharArray()
    |> Array.ofSeq
    
let Part1 () =
    let arr = calc1 inputStrings
    arr |> Seq.iter (String >> printfn "%s")
    
    ArrayUtils.allWhere (fun x->x='x') arr
    |> Seq.length
    |> printfn "%d"
    
let rec calcRemove arr =
    //printArr arr
    let c=
        arr
        |> ArrayUtils.allWhere (fun x->x='@')
        |> Seq.length
    let arr2 = calc1 arr
    let c2 =
        arr2
        |> ArrayUtils.allWhere (fun x->x='@')
        |> Seq.length
    if c2=c then
        arr
    else
        calcRemove arr2
    
let calc2 arr =
    let originalCount =
        arr
        |> ArrayUtils.allWhere (fun x->x='@')
        |> Seq.length
        
    let finalCount = 
        calcRemove arr 
        |>  ArrayUtils.allWhere (fun x->x='@')
        |> Seq.length
    originalCount-finalCount
let Part2 () =
    calc2 inputStrings
    |> printfn "%d"
