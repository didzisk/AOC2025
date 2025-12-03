module Dec03

open System.IO
open StringUtils

let day = 3

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings =
    File.ReadAllLines filename

let maxDigitLeavingNPositions (start:int) (n:int) (s:string) =
    s[start..s.Length-1-n]
    |> Seq.maxi //index of and max item itself
    |> fun (i,m) -> i + start + 1, m  
        
let folder s (pos, curr) n =
    let newPos, m = maxDigitLeavingNPositions pos n s
    newPos, curr * 10L + (c2l m) //c2l = charToInt64
    
let calcLine d s =
    List.rev [0..d] //requiredLength-1 downto 0 
    |> List.fold (folder s) (0,0)
    |> snd
    
let Calc () =

    inputStrings
    |> Seq.map (calcLine 1)
    |> Seq.sum
    |> printfn "Part1: %A"

    inputStrings
    |> Seq.map (calcLine 11)
    |> Seq.sum
    |> printfn "Part2: %A"