module Dec02

open System.IO
open FParsec

let day = 2

let filename = MetaUtils.todayFilename day

let inputString =
    File.ReadAllText filename

type Interval = int64 * int64

let pint = pint64 .>> spaces

let intervalParser: Parser<Interval, unit> =
    pint .>>. (pchar '-' >>. pint)

let intervalsParser: Parser<Interval list, unit> =
    sepBy intervalParser (pchar ',')

let parseIntervals input =
    match run intervalsParser input with
    | Success(result, _, _) -> result
    | Failure(err, _, _)    -> failwith err

let allPartsEqual arr =
    (Seq.length arr > 1) && (arr |> Seq.distinct |> Seq.length = 1)
        
let splitIntoEqualLengths (s:string) (n:int) =
    let len = s.Length
    if len % n <> 0 then
        Seq.empty
    else
        let p = len / n //part length
        seq {
            for i = 1 to n do
                yield s[(i-1)*p..i*p-1]
        }
        
let consistsOfEqualParts (s:string) =
    seq {1..s.Length}
    |> Seq.map ((splitIntoEqualLengths s) >> allPartsEqual)
    |> Seq.exists id

let consistsOf2EqualParts (s:string) =
    splitIntoEqualLengths s 2
    |> allPartsEqual
    
let sumIntegersWith2EqualParts(lines:Interval seq) =
    seq{
        for (a,b) in lines do
            for i in [a .. b] do
                if consistsOf2EqualParts (i.ToString()) then
//                    printfn "%d" i
                    yield i
                else
                    0L
        
    }
    |> Seq.sum     

let sumIntegersWithEqualParts(lines:Interval seq) =
    seq{
        for (a,b) in lines do
            for i in [a .. b] do
                if consistsOfEqualParts (i.ToString()) then
//                    printfn "%d" i
                    yield i
                else
                    0L
        
    }
    |> Seq.sum     

let Part1 () =
    // Example
    let ex = parseIntervals "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

    ex |> sumIntegersWith2EqualParts |> printfn "Ex1 %A"
    
    let parsed = (parseIntervals inputString)

    parsed |> sumIntegersWith2EqualParts |> printfn "Part1 %A"
    parsed |> sumIntegersWithEqualParts |> printfn "Part2 %A"

