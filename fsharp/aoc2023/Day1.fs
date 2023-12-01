module aoc2023.Day1

open System.Text.RegularExpressions
open Shared


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
   
   
let findNr word =
    let candidates = Map.ofList replacements
    
    let rec findRec gathered toScan =
        match toScan with
        | "" -> 0
        | _ ->
            let next = toScan.Substring(0,1)            
            match next with    
            | ParseRegex "(\\d)" [d] ->
                int d
            | _ ->
                let rest = toScan.Substring(1)
                let gatheredNew = gathered + next
                
                let found = Seq.tryFind (fun (c: string) -> gatheredNew.Contains(c)) (Map.keys candidates)
                match found with
                | Some c -> Map.find c candidates
                | None ->
                    findRec gatheredNew rest                    
    findRec "" word

let findNrRight (word: string) =
    let candidates = Map.ofList replacements
    
    let rec findRec gathered toScan =
        match toScan with
        | "" -> 0
        | _ ->
            let next = toScan.Substring(0,1)            
            match next with    
            | ParseRegex "(\\d)" [d] ->
                (int d)
            | _ ->
                let rest = toScan.Substring(1)
                let gatheredNew = next + gathered 
                
                let found = Seq.tryFind (fun (c: string) -> gatheredNew.Contains(c)) (Map.keys candidates)
                match found with
                | Some c -> (Map.find c candidates)
                | None ->
                    findRec gatheredNew rest
                    
    let reverted = word.ToCharArray() |> Array.rev |> System.String
    
    findRec "" reverted         
    
   

let findNrs word =
    (findNr word, findNrRight word)

let toNr (left, right) = 10 * left + right

let day1 lines =
    List.map (findNrs >> toNr) lines
    |> List.sum

let day1b lines =
    let nrs = List.map (findNrs >> toNr) lines
    
    printfn "%A" nrs
    
    let pluslog total nr =
        printfn "Total: %d" (total + nr)
        total + nr
    
    List.fold pluslog 0 nrs
    
    
