module Dec09

open System
open System.IO
open FParsec

let filename = MetaUtils.getTodayInput 9

let inputStrings =
    File.ReadAllLines filename
    
type Pos = {x:int64;y:int64}

let ppos= pipe2 (pint64 .>> pchar ',') pint64 (fun x y ->{Pos.x=x; y=y})
    
let parseInput (lines:string array) =
    lines
    |> Array.map (fun s ->
        match run ppos s with
        | Success(r,_,_)-> r
        | _ -> failwith "unexpected"
        )
    
let area (a:Pos) (b:Pos) =
    (abs(a.x-b.x) + 1L) * (abs(a.y-b.y) + 1L) 

let allAreas (lines:string array) =
    let allPos = parseInput lines
    [|
        for i = 0 to allPos.Length-1 do
            for j = i+1 to allPos.Length-1 do
                yield area allPos[i] allPos[j]  
    |]
    
let border (lines:string array) = 
    let allPos = parseInput lines
    [|
        yield! allPos |> Array.pairwise
        yield allPos[0],allPos[allPos.Length-1]
    |]
    
let boxCrossesLine ((box1,box2):Pos*Pos) ((line1,line2):Pos*Pos) =
    let left = min box1.x box2.x
    let right = max box1.x box2.x
    let top = min box1.y box2.y
    let bottom = max box1.y box2.y
    let lineRight = max line1.x line2.x
    let lineLeft = min line1.x line2.x
    let lineBottom = max line1.y line2.y
    let lineTop = min line1.y line2.y
    let avoids = 
         lineRight <= left
        || right <= lineLeft
        || lineBottom <= top
        || bottom <= lineTop
    not avoids
    
let boxAvoidsLines border (box:Pos*Pos) =
    border
    |> Seq.exists (boxCrossesLine box)
    |> not
    
let calc2 (lines:string array) =
    let borders = border lines
    let allPos = parseInput lines
    [|
        for i = 0 to allPos.Length-1 do
            for j = i+1 to allPos.Length-1 do
                if boxAvoidsLines borders (allPos[i],allPos[j]) then
                    yield area allPos[i] allPos[j]  
    |]

let Calc() =
    // inputStrings
    // |> Array.iter (printfn "%s")

    printfn ""
    
    inputStrings
    |> allAreas
    |> Array.max
    |> printfn "%d"

    let ex = File.ReadAllLines (MetaUtils.todayFilenameEx 9)

    calc2 ex
    |> Array.max
    |> printfn "Part2ex: %d"
    
    calc2 inputStrings
    |> Array.max
    |> printfn "Part2: %d"