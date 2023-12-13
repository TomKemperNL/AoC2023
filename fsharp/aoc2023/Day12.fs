module aoc2023.Day12

open Shared

type Gear =
    | Operational
    | Damaged

type RecordGear =
    | Unknown
    | Gear of Gear

type Record = {
    Gears: RecordGear list
    Damages: int list
}

let parseLine line =
        match line with
        | ParseRegex "(.+) (.+)" [gears; damages] ->
            let parseChar c =
                match c with
                | '?' -> Unknown
                | '.' -> Gear Operational
                | '#' -> Gear Damaged
                | _ -> failwith $"unable to parse %A{c}"
            {
                Gears = gears.ToCharArray() |> Array.toList |> List.map parseChar
                Damages = damages.Split(",") |> Array.toList |> List.map int 
            }
        | _ -> failwith $"unable to parse %s{line}"

let rec parse (input: string list) : Record list =
    List.map parseLine input

let forceKnown recordGears =
    let toGear rg =
        match rg with
        | Unknown -> failwith "shouldn't have any unknowns left"
        | Gear g -> g
    List.map toGear recordGears

let backFillUnknowns contiguousRecords recordGear =
    
    let rec walk todo remainingGear isFilling results =
        match todo with
        | 0 -> results
        | _ ->
            match remainingGear with
            | [] -> failwith "wuuut?"
            | Unknown :: t ->
                if isFilling then
                    let newResults = List.map (fun r -> Gear Damaged :: r) results     
                    walk (todo - 1) t true newResults
                else
                    let toFill = List.map (fun r -> Gear Damaged :: r) results
                    let notToFill = List.map (fun r -> Gear Operational :: r) results
                    List.concat [walk (todo - 1) t true toFill; walk (todo) t false notToFill ]                
            | Gear Operational :: t ->
                let newResults = List.map (fun r -> Gear Operational :: r) results
                walk (todo) t false newResults
            | Gear Damaged :: t ->
                let newResults = List.map (fun r -> Gear Damaged :: r) results
                walk (todo - 1) t true newResults
    
    walk contiguousRecords recordGear false [[]]
    
let arrangements (record : Record) : RecordGear list list =
    let computePermutations permutations dams =
        List.collect (backFillUnknowns dams) permutations
    
    List.fold computePermutations [List.rev record.Gears] (List.rev record.Damages)
    
let day12a (records : Record list) =
    List.map arrangements records |> List.map List.length |> List.sum
    
let day12b = day12a   