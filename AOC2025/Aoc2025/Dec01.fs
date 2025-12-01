module Dec01

open System
open System.IO
open FParsec

let day = 1

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings =
    File.ReadAllLines filename

type Command =
    | CLeft of int
    | CRight of int

let pLeft = pstring "L" >>. pint32 |>> CLeft
let pRight = pstring "R" >>. pint32 |>> CRight
let pLine: Parser<Command,unit> = pLeft <|> pRight

let commands lines = 
    lines
    |> Seq.map (fun l->
        match run pLine l with
        | Success(command, _, _) -> command
        
        | Failure(s, parserError, unit) -> failwith "todo"

        )
    
let Part1 () =
//    inputStrings    |> Seq.iter (printfn "%s")
    let mutable state = 50    
    seq {
        for c in (commands inputStrings) do
            match c with
            | CLeft i ->
                state <- state - i
            | CRight i ->
                state <- state + i
            if state % 100 = 0 then
                yield 0
    }
    |> Seq.length
    |> printfn "Part1: %d"

let abs a = 
    match a with
    | i when i<0 -> -i
    | i -> i
    
let inline (%!) a b = (a % b + b) % b

let ex =
    [|
        "L68"
        "L30"
        "R48"
        "L5"
        "R60"
        "L55"
        "L1"
        "L99"
        "R14"
        "L82"        
    |]

let Solve2 lines  =
    let mutable state = 50    
    seq {
        for c in commands lines do
            let rotations, newState =
                match c with
                | CLeft i ->
                    if state = 0 then
                        state<-100
                    if state>i then
                        0, state - i
                    else
                        let rot = (i-state) / 100 + 1
                        let n = state - i + 100 * rot
                        rot, n
                | CRight i ->
                    if state = 100 then
                        state<-0
                    let rot = (i+state) / 100
                    let n = i+state - rot * 100
                    rot, n
            printfn "%A state: %d rotations: %d newState: %d" c state rotations newState
            if state<0 then
                failwith "<0"
            if state>100 then
                failwith ">100"
            state <- newState
            yield rotations
    }
    |> Seq.sum
    |> printfn "Part2: %d"

let ex2 () =
    Solve2 ex
    
let Part2 () =
    Solve2 inputStrings