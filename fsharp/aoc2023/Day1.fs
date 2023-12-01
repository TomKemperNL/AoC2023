module aoc2023.Day1

open System.Text.RegularExpressions



let numbers (word: string) : int list=
    let matches = List.ofSeq (Regex.Matches(word, "\\d"))    
    List.map (fun (m: Match) -> m.Value) matches
    |> List.map int

let firstLast nrs =
    (List.head nrs, List.head (List.rev nrs)) //Performance Shmerformance

let toNr (left, right) = 10 * left + right

let day1 lines =
    List.map (numbers >> firstLast >> toNr) lines
    |> List.sum
