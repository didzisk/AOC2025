module Dec06


open System
open System.IO
open StringUtils

let filename = MetaUtils.getTodayInput 6

let inputStrings =
    File.ReadAllLines filename
        
let Calc1 (lines: string array)   =
    let arr =
        lines
        |> Array.map _.Split([|' '|], StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    let res = Array.create arr[0].Length 0L
    for c=0 to arr[0].Length-1 do
        let op = arr[arr.Length-1][c]
        let seed =
            match op with
            | "+" -> 0L
            | "*" -> 1L
            | _ -> failwith "unexpected op"
        res[c]<-seed
        for r=0 to arr.Length-2 do 
            match op with
            | "+" -> res[c] <- res[c] + int64 (arr[r][c])
            | "*" -> res[c] <- res[c] * int64 (arr[r][c])
            | _ -> failwith "unexpected op"
    res
    |> Array.sum
        
let addArg (lines: string array) (col:int) (seed:int64) (op:char)=
    let arr =
        [|
            for r = 0 to lines.Length-1 do
                let  n = lines[r][col] 
                if n |> System.Globalization.CharUnicodeInfo.GetDigitValue >=0 then
                    yield n
        |]
    if arr.Length>0 then
        let arg = int64 (String arr)
        match op with
        | '*' -> seed * arg
        | '+' -> seed + arg
        | _ -> failwith "wrong operator"
    else
        seed
    
    
let Calc2 (lines: string array) =
    let args =
        seq{
            let mutable res = 0L
            let mutable op = ' '
            for c = 0 to lines[0].Length-1 do
                match lines[lines.Length-1][c] with
                | '*' ->
                    yield res
                    res <- 1L
                    op <- '*'
                | '+' ->
                    yield res
                    res <- 0L
                    op <- '+'
                | _ -> ()
                res <- addArg lines c res op
                if c = lines[0].Length-1 then
                    yield res
        }
    args
    |> Seq.sum


let Calc () =
    Calc1 inputStrings
    |> printfn "Part1 %d"

    Calc2 inputStrings
    |> printfn "Part2 %d"