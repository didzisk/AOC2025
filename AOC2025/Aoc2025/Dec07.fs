module Dec07

open System
open System.IO
open StringUtils
open ArrayUtils

let filename = MetaUtils.getTodayInput 7

let inputStrings =
    File.ReadAllLines filename

let doSplits (lines:string array) =
    let arr =
        lines
        |> Array.map _.ToCharArray()
    let arr2 = 
        Array.init arr.Length (fun _ -> Array.zeroCreate<int64> arr[0].Length)
    for r = 0 to arr.Length-2 do
        for c = 0 to arr[0].Length-1 do
            match arr[r][c] with
            | 'S' ->
                arr[r+1][c]<-'|'
                arr2[r+1][c]<-1L
            | '|' ->
                match arr[r+1][c] with
                | '^' ->
                    if validPos arr (r+1,c-1) then
                        arr2[r+1][c-1]<-arr2[r+1][c-1]+arr2[r][c]
                        arr[r+1][c-1]<-'|'
                        
                    if validPos arr (r+1,c+1) then
                        arr2[r+1][c+1]<-arr2[r+1][c+1]+arr2[r][c]
                        arr[r+1][c+1]<-'|'
                | _ ->
                    if validPos arr (r+1,c) then
                        arr2[r+1][c]<-arr2[r+1][c]+arr2[r][c]
                        arr[r+1][c]<-'|'
            | _ -> ()
    arr, arr2

let calc1 (arr: char array array) =
    seq {
        for r = 2 to arr.Length-1 do
            for c = 0 to arr[0].Length-1 do
                if arr[r][c] = '^' then
                    if arr[r-1][c] = '|' then
                        yield 1                 
    }
    |> Seq.sum
    
let calc2 (arr: int64 array array) =
    arr[arr.Length-1]
    |> Array.sum
    
let Calc () =
    let ex = File.ReadAllLines (MetaUtils.todayFilenameEx 7)
    let arrEx, arrEx2 =
        doSplits ex
    arrEx
        |> Array.map String
        |> Array.iter (printfn "%s")    
        
    arrEx2    
        |> Array.iter (printfn "%A")
        
    (calc1 arrEx) |> (printfn "Ex1 %d")
    (calc2 arrEx2)|> (printfn "Ex2 %d")
    // inputStrings
    // |> Array.iter (printfn "%s")
    
    let arr1, arr2 = doSplits inputStrings
    arr1
        |> Array.map String
        |> Array.iter (printfn "%s")    
        
    (calc1 arr1) |> (printfn "Part1 %d")
    
    (calc2 arr2) |> (printfn "Part2 %d")
    