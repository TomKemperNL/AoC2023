module aoc2023.Day9

open Microsoft.FSharp.Core

type History = int64 list

let rec parseLine (line: string) : History =
    line.Trim().Split(" ") |> Array.map int64 |> List.ofArray

let parse lines : History list =
    List.map parseLine lines

let differences hist =        
        let pairs = List.pairwise hist
        List.map (fun (a,b) -> b - a) pairs

let rec collectDifferences hist : int64 list list=
    let diffs = differences hist
    if List.forall ((=) 0L) diffs then
        [diffs]
    else
        diffs :: collectDifferences diffs 

let predictNext (history: History) : int64 =    
    let difList : int64 list list = collectDifferences history
    let rec predictNext (diffs: int64 list list) =
        match diffs with
        | [] -> 0L
        | d :: rest ->
            List.last d + (predictNext rest)
    
    predictNext (history :: difList)

let rec predictPrev (history: History) : int64 =
    let difList : int64 list list = collectDifferences history
    let rec predictPrev (diffs: int64 list list) =
        match diffs with
        | [] -> 0L
        | d :: rest ->
            List.head d - (predictPrev rest)
            
    predictPrev (history :: difList)

let day9a input =
    List.map predictNext input |> List.sum
    
let day9b input =
    List.map predictPrev input |> List.sum