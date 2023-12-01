module aoc2023.Day1

open System.Text.RegularExpressions



let numbers (word: string) : int list=
    let matches = List.ofSeq (Regex.Matches(word, "\\d"))    
    List.map (fun (m: Match) -> m.Value) matches
    |> List.map int
    

let replacements =
    [
        ("one", 1)
        ("two", 2)
        ("three", 3)
        ("four", 4)
        ("five", 5)
        ("six", 6)
        ("seven", 7)
        ("eight", 8)
        ("nine", 9)        
    ]
   
let replaceNrs (word: string) : string =     
    
    let replaceAll w =
        let replace (w:string) (t,r) : string =
            w.Replace(t, string(r))    
        List.fold replace w replacements    
    
    let rec replaceLeft gathered word =
        match word with
        | "" ->
            gathered
        | _ ->
            let rest = word.Substring(1)
            let ch = word.Substring(0,1)
            replaceLeft (replaceAll (gathered + ch)) rest
    
    replaceLeft "" word
    
    

let firstLast nrs =
    (List.head nrs, List.head (List.rev nrs)) //Performance Shmerformance

let toNr (left, right) = 10 * left + right

let day1 lines =
    List.map (numbers >> firstLast >> toNr) lines
    |> List.sum

let day1b lines =
    List.map (replaceNrs >> numbers >> firstLast >> toNr) lines
    |> List.sum
