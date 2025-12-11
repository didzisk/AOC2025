module Dec08

open System
open System.IO
open FParsec
open ArrayUtils

let filename = MetaUtils.getTodayInput 8

let inputStrings =
    File.ReadAllLines filename

type Pos = {x:int64;y:int64;z:int64}

let ppos= pipe3 (pint64 .>> pchar ',') (pint64 .>> pchar ',') pint64 (fun x y z ->{Pos.x=x; y=y; z=z})
    
let parseInput (lines:string array) =
    lines
    |> Array.map (fun s ->
        match run ppos s with
        | Success(r,_,_)-> r
        | _ -> failwith "unexpected"
        )
    
let distSquared (a:Pos) (b:Pos) =
    (a.x-b.x)*(a.x-b.x) + (a.y-b.y)*(a.y-b.y) + (a.z-b.z)*(a.z-b.z)
    
let allDistances (lines:string array) =
    let allPos = parseInput lines
    [|
        for i = 0 to allPos.Length-1 do
            for j = i+1 to allPos.Length-1 do
                yield distSquared allPos[i] allPos[j],(min i j,max i j)  
    |]
    |> Array.sortBy fst 

let makeSet (parent:int array) (v:int) =
    parent[v]<-v
    
let rec findSet(parent:int array) (v:int) =
    if v=parent[v] then
        v
    else
        let rootParent = findSet parent parent[v]
        parent[v]<-rootParent //ugly F#, ugh!
        rootParent
        
let unionSets (parent:int array) (a:int) (b:int) =
    let setHeadA = findSet parent a
    let setHeadB = findSet parent b
    if setHeadA <> setHeadB then
        parent[setHeadB] <- setHeadA
        for i = 0 to parent.Length-1 do
            if parent[i] = setHeadB then
                parent[i] <- setHeadA
        
let calc1ex (lines:string array) =
    let arr = allDistances lines
    // arr
    // |> Array.iter (printfn "%A")

    printfn "***************************************"    
    let parent = Array.init 40 id
    for i = 0 to 9 do
        let d,(a,b) = arr[i]
        printfn "Dist: %d nodes %d %d" d a b
        unionSets parent a b
        
    let grouped = 
        parent
        |> Array.groupBy id
        |> Array.map (fun (c,a) -> c, Array.length a )
        
    //grouped |> Array.iter (printfn "%A")
    
    grouped
    |> Seq.sortByDescending snd
    |> Seq.take 3
    //|> Seq.iter (printfn "%A")
    |> Seq.fold (fun n (_,b) -> n*b) 1
    |> printfn "Part1ex %d"
  
    // parent
    // |> Array.iter (printfn "%A")
    
let ex = File.ReadAllLines (MetaUtils.todayFilenameEx 8)

let calc1 (lines:string array) =
    let arr = allDistances lines
    // arr
    // |> Array.iter (printfn "%A")

    printfn "***************************************"    
    let parent = Array.init lines.Length id
    for i = 0 to 999 do
        let d,(a,b) = arr[i]
        unionSets parent a b
        
    let grouped = 
        parent
        |> Array.groupBy id
        |> Array.map (fun (c,a) -> c, Array.length a )
        
    //grouped |> Array.iter (printfn "%A")
    
    grouped
    |> Seq.sortByDescending snd
    |> Seq.take 3

    |> Seq.fold (fun n (_,b) -> n*b) 1
    |> printfn "Part1 %d"

let calc2 (lines:string array) =
    let arr = allDistances lines
    let allPos = parseInput lines

    printfn "***************************************"    
    let parent = Array.init lines.Length id
    for i = 0 to arr.Length do
        let d,(a,b) = arr[i]
        unionSets parent a b
        let numClusters =
            parent
            |> Array.distinct
            |> Array.length
        if numClusters = 1 then
            printfn "Part2: %d" (allPos[a].x * allPos[b].x)
            exit 0 //Ugly F#

let Calc () =
    // inputStrings
    // |> Array.iter (printfn "%s")

        //calc1ex ex
    calc1 inputStrings
    calc2 inputStrings
