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
                yield distSquared allPos[i] allPos[j],(i,j)  
    |]
    |> Array.sortBy fst 

let makeSet (parent:int array) (v:int) =
    parent[v]<-v
    
let rec findSet(parent:int array) (v:int) =
    if v=parent[v] then
        v
    else
        findSet parent parent[v] //compress: put this into parent[v] 
        
let unionSets (parent:int array) (a:int) (b:int) =
    let setHeadA = findSet parent a
    let setHeadB = findSet parent b
    if setHeadA <> setHeadB then
        parent[setHeadB] <- setHeadA
        
let calc1ex (lines:string array) =
    let arr = allDistances lines
    let parent = Array.init 40 id
    for i = 0 to 9 do
        let d,(a,b) = arr[i]
        printfn "Dist: %d nodes %d %d" d a b
        unionSets parent a b
        
    parent
    |> Array.groupBy id
    |> Array.map (fun (c,a) -> c, Array.length a )
    |> Array.iter (printfn "%A")
  
    parent
    |> Array.iter (printfn "%A")
    
    
(*
void make_set(int v) {
    parent[v] = v;
}

int find_set(int v) {
    if (v == parent[v])
        return v;
    return find_set(parent[v]);
}

void union_sets(int a, int b) {
    a = find_set(a);
    b = find_set(b);
    if (a != b)
        parent[b] = a;
}
*)

let ex = File.ReadAllLines (MetaUtils.todayFilenameEx 8)


let Calc () =
    // inputStrings
    // |> Array.iter (printfn "%s")
    //
    // inputStrings
    // |> allDistances
    // |> Array.length
    // |> printfn "%d"
    
    calc1ex ex
