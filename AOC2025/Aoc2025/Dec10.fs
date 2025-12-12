module Dec10

open System.IO
open Microsoft.Z3

let filename = MetaUtils.getTodayInput 10

let inputStrings =
    File.ReadAllLines filename
    
type Machine =
    {
        Lights: int
        Switches: int list
        Switches2: int list list
        Joltages: int list
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
    let switches2 =
        [
            for s in arr[1..arr.Length-2] do
                yield
                    s[1..s.Length-2].Split [|','|]
                    |> Seq.map int
                    |> List.ofSeq
        ]
    let joltages =
        let s = arr[arr.Length-1]
        s.[1..s.Length-2].Split [|','|]
        |> Seq.map int
        |> List.ofSeq
    {Machine.Lights = lights; Switches = switches; Switches2 = switches2; Joltages = joltages}

    

let parse (lines:string array) =
    lines
    |> Array.map parseOneLine
    
let doSwitch (lights:int) (switch:int) =
    lights ^^^ switch
    
let applyAllSwitches (lights:int) (switches:int list) =
    switches |> List.fold doSwitch 0
        

[<TailCall>]
let rec subsets list =
    match list with
    | [] -> [[]] // Base case: an empty list has one subset, which is the empty set
    | head :: tail ->
        let subsetsOfTail = subsets tail
        // For each subset of the tail, we can either include the head or not
        subsetsOfTail @ (List.map (fun s -> head :: s) subsetsOfTail)
    
let calc1line (m: Machine)=
    subsets m.Switches
    |> Seq.map (fun x->
        let lights = applyAllSwitches m.Lights x
        if lights = m.Lights then
            x.Length
        else
            1000
        )
    |> Seq.min
    
let calc1 (machines:Machine array) =
    machines
    |> Seq.map calc1line
    |> Seq.sum
    
let calc2line (m:Machine) =
    (*  lights [.........switches............] [target joltages]
                0    1    2    3     4    5     0 1 2 3
    ex: [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    m.Joltages[0] = x4+x5+0x1+0x2+0x3
    m.Joltages[1] = x1+x5
    m.Joltages[2] = x2+x3+x4
    m.Joltages[3] = x0+x1+x3
    
    step (buttonNumber) = 
    
    *)
    m.Switches2
    0
    
let calc2 (machines:Machine array) =
    machines
    |> Seq.map calc2line
    |> Seq.sum
    
let Calc() =
    // inputStrings
    // |> parse
    // |> Array.iter (printfn "%A")
    inputStrings
    |> parse
    |> calc1
    |> printfn "Part 1: %d"