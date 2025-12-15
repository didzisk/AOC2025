module Dec10

open System
open System.Collections.Generic
open System.IO
open Microsoft.Z3

let filename = MetaUtils.getTodayInput 10

let inputStrings =
    File.ReadAllLines filename
    
type Machine =
    {
        Lights: int
        Switches: int list
        Joltages: int array
    }

let parseOneLine (line:string) =
    let arr = line.Split [|' '|]
    let lights =
          seq{for i in 1..arr[0].Length-2 do
              if arr[0][i] = '#' then
                  yield 1 <<< (i-1)
          }
          |> Seq.sum
          
    let switches =
        [
            for s in arr[1..arr.Length-2] do
                yield
                    s[1..s.Length-2].Split [|','|]
                    |> Seq.map (fun c ->
                        let i  = int c
                        1<<<i
                        )
                    |> Seq.sum
        ]
    let joltages =
        let s = arr[arr.Length-1]
        s.[1..s.Length-2].Split [|','|]
        |> Seq.map int
        |> Array.ofSeq
    {Machine.Lights = lights; Switches = switches; Joltages = joltages}

    

let parse (lines:string array) =
    lines
    |> Array.map parseOneLine
    
let doSwitch (lights:int) (sw:int) =
    lights ^^^ sw
    
let applyAllSwitches (switches:int list) =
    switches |> List.fold doSwitch 0

let rec subsets list =
    match list with
    | [] -> [[]] // Base case: an empty list has one subset, which is the empty set
    | head :: tail ->
        let subsetsOfTail = subsets tail
        // For each subset of the tail, we can either include the head or not
        subsetsOfTail @ (List.map (fun s -> head :: s) subsetsOfTail)
    
let calcToTargetPattern (switches:int list) (target :int)=
    seq{
        for x in subsets switches do
            let lights = applyAllSwitches x
            if lights = target then
                yield x.Length, x
    }
    |> Seq.sortBy fst

let calc1line (m: Machine)=
    calcToTargetPattern m.Switches m.Lights |> Seq.head |> fst
    
let calc1 (machines:Machine array) =
    machines
    |> Seq.map calc1line
    |> Seq.sum

let oddJoltages (joltages: int array)=
    //convert the odd items into "lights", apply calculation for lights
    let odds =
        seq{for i in 0..joltages.Length-1 do
            if joltages[i] % 2 = 1  then
                yield 1 <<< i
            }
        |> Seq.sum
    odds

let applySwitch (joltages: int array) (sw:int)  =
    //printf "("
    let newJoltages =
        [|
            for i = 0 to joltages.Length-1 do
                let mask = (1 <<< i)
                if sw &&& mask = mask then
                    yield joltages[i]-1
      //              printf "%d" i
                else
                    yield joltages[i]
        |]
    //printf ")"
    newJoltages
            
let applySwitches (switches:int list) (joltages: int array) =
    switches |> List.fold applySwitch joltages
    
[<TailCall>]
let rec calcStep (memo:Dictionary<int array,int>)(switches:int list) (joltages: int array) : int =
    if joltages |> Array.exists (fun x-> x<0) then
        1_000_000
    else if memo.ContainsKey joltages then
            memo[joltages]
    else
        let total = 
            let thisStepCombinations = calcToTargetPattern switches (oddJoltages joltages) //list of instructions
            if (Seq.length thisStepCombinations) = 0 then
                1_000_000
            else
                thisStepCombinations
                |> Seq.map (fun (n, newSwitches) -> 
                    let modifiedJoltages = applySwitches newSwitches joltages //now I have joltages to modify in next step, I will use the original switches again
                    if (modifiedJoltages |> Array.forall(fun x-> x=0)) then 
                        n
                    else
                        let nextJoltages =
                            modifiedJoltages |> Array.map (fun x->x/2) //integer div 2
                        let subN = calcStep memo switches nextJoltages
                        n + 2 * subN
                    )
                |> Seq.min
        memo[joltages] <- total
        total
        
let calc2line (m:Machine) =
    let memo = Dictionary<(int array),int>()
    printfn "%A" m
    let t = calcStep memo m.Switches m.Joltages
    printfn "%d." t 
    t
    
let calc2 (machines:Machine array) =
    printfn ""
    machines
    |> Seq.map calc2line
    |> Seq.map int64
    |> Seq.sum
    
let Calc() =
    // inputStrings
    // |> parse
    // |> Array.iter (printfn "%A")
    inputStrings
    |> parse
    |> calc1
    |> printfn "\nPart 1: %d"


    [|    
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    |]
        |> parse
        |> calc2
        |> printfn "\nEx 2: %d"

    //exit 0
    
    inputStrings
    |> parse
    |> calc2
    |> printfn "\nPart 2: %d"

