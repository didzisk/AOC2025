module Dec11

open System.Collections.Generic
open System.IO

let filename = MetaUtils.getTodayInput 11

let inputStrings =
    File.ReadAllLines filename

let parse (lines:string array) =
    let arr =
        seq{
        for s in lines do
            let arr = s.Split [|' '|]
            let name = arr[0][0..2]
            let others = arr[1..] |> List.ofArray
            name, others
    }
//    arr |> Seq.iter (printfn "%A")
    printfn "******************************************************************"
    arr |> Map.ofSeq

//Directed Acyclic Graph,
//count all paths from source to target in a DAG using DFS + memoization
let countPathsInDag1 (graph:Map<'a, 'a list>) (source:'a) (target:'a)  =
    let memo = Dictionary<'a, int64>() // Cache results for each node

    // Recursive DFS function
    let rec dfs node =
        // If reached target, there is exactly 1 path (the current one)
        if node = target then 1L
        // If already computed, return cached value
        elif memo.ContainsKey(node) then memo[node]
        else
            // Sum paths from all neighbors
            let total =
                graph.[node]
                |> List.sumBy dfs
            memo.[node] <- total
            total

    dfs source

//Directed Acyclic Graph,
//count all paths from source to target in a DAG using DFS + memoization
let countPathsInDag2 (graph:Map<'a, 'a list>) (source:'a) (target:'a)  =
    let memo = Dictionary<'a * bool * bool, int64>() // Cache results for each node

    // Recursive DFS function
    // DFS with state tracking
    let rec dfs node has1 has2 =
        let has1new = has1 || node = "fft"
        let has2new = has2 || node = "dac"
        // If reached target, there is exactly 1 path (the current one)
        if node = target then
            if has1new && has2new then 1L else 0L
        // If already computed, return cached value
        elif memo.ContainsKey(node, has1new, has2new) then
            memo[node,has1new,has2new]
        else
            // Sum paths from all neighbors
            let total =
                graph.[node]
                |> List.sumBy (fun next -> dfs next has1new has2new)
                |> int64
            memo[(node, has1new, has2new)] <- total
            total
    dfs source false false

let count1 lines =
    let nodes = parse lines
    countPathsInDag1 nodes "you" "out"

let count1ex lines =
    let nodes = parse lines
    countPathsInDag1 nodes "svr" "out"

let count2 lines =
    let nodes = parse lines
    countPathsInDag2 nodes "svr" "out"

let Calc() =
    printfn "-----------------------------------------------------"
    count1 inputStrings
    |> (printfn "Part1: %d")
    printfn "-----------------------------------------------------"
    count1ex inputStrings
    |> (printfn "Part1ex: %d")
    
    printfn "-----------------------------------------------------"
    count2 inputStrings
    |> (printfn "Part2: %d")