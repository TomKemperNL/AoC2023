module aoc2023.Day12

open System.Collections.Generic
open Shared

type Gear =
    | Operational
    | Damaged
    | Unknown

module Gear =        
    let toString (gears: Gear list) =
        let toChar g =
            match g with
            | Unknown -> '?'
            | Operational -> '.'
            | Damaged -> '#'
        System.String(List.map toChar gears |> List.toArray)

    let ofString (gears: string) =
        let parseChar c =
                    match c with
                    | '?' -> Unknown
                    | '.' -> Operational
                    | '#' -> Damaged
                    | _ -> failwith $"unable to parse %A{c}"
        gears.ToCharArray() |> Array.toList |> List.map parseChar
    
    let combine (g1s,g2s) =
        List.append g1s g2s

type Record = {
    Gears: Gear list
    Damages: int list
}


let parseLine line =
        match line with
        | ParseRegex "(.+) (.+)" [gears; damages] ->            
            {
                Gears = Gear.ofString gears
                Damages = damages.Split(",") |> Array.toList |> List.map int 
            }
        | _ -> failwith $"unable to parse %s{line}"

let rec parse (input: string list) : Record list =
    List.map parseLine input

let generatePossibleSolutionsBackfill contiguousRecords recordGear =
    let reversed = List.rev recordGear
    let rec walk todo remainingGear isFilling results =
        match todo with
        | 0 ->
            match remainingGear with
            | Damaged :: _  -> [] //no todos, but the next one is also a damaged gear, so no useful solutions in this branch
            | Unknown :: t ->
                let newResults = List.map (fun r -> Operational :: r) results
                List.map (fun r -> List.rev t, r) newResults
            | _ ->                
                List.map (fun r -> List.rev remainingGear, r) results            
        | _ ->
            match remainingGear with
            | [] -> [] //still have to replace things, but no gear left, so no useful solutions in this branch
            | Unknown :: t ->                
                if isFilling then
                    let newResults = List.map (fun r -> Damaged :: r) results     
                    walk (todo - 1) t true newResults
                else
                
                    let toFill = List.map (fun r -> Damaged :: r) results
                    let notToFill = List.map (fun r -> Operational :: r) results
                    List.concat [walk (todo - 1) t true toFill; walk (todo) t false notToFill ]
               
            | Operational :: t ->
                if isFilling then
                    []
                else
                    let newResults = List.map (fun r -> Operational :: r) results
                    walk (todo) t false newResults
            | Damaged :: t ->
                let newResults = List.map (fun r -> Damaged :: r) results
                walk (todo - 1) t true newResults
    
    walk contiguousRecords reversed false [[]]


let rec fillRemainder gears =
    match gears with
    | [] -> []
    | h :: t ->
        match h with
        | Unknown -> Operational :: fillRemainder t
        | g -> g :: fillRemainder t 
    
let arrangements (record : Record) : Gear list list =
    let rec computePermutations (rem: Gear list) dams =
       match dams with
       | [] ->
           if List.exists ((=) Damaged) rem then []
           else [fillRemainder rem]
       | h :: t ->
           let partialSolutions = generatePossibleSolutionsBackfill h rem
           match partialSolutions with
           | [] -> []
           | _ ->
               let expandSolution (rem, solved) =
                   let recurSolutions = computePermutations rem t
                   List.map (fun rs -> List.append rs solved) recurSolutions
               List.collect expandSolution partialSolutions
           
    let results = computePermutations record.Gears (List.rev record.Damages)
    results |> List.distinct

let isSolution record candidate =
    let lengthSame =
        List.length candidate = List.length record.Gears
    
    let totalGearsOk =
        let expected =List.sum record.Damages
        let found = List.filter ((=) Damaged) candidate |>List.length
        expected = found
    //
    // let rec gearStructureOk candidate damages =
    //     match damages with
    //     | [] -> List.none ((=) Damaged) candidate 
    //     | d :: t ->
    //         match candidate with
    //         | [] -> false
    //         | Unknown :: _ -> failwith "Oi!"
    //         | Operational :: t ->
    //             gearStructureOk 
        
    List.forall id [lengthSame; totalGearsOk]
    
let day12a (records : Record list) =
    let arrangements = List.mapi (fun ix r -> ix,r,arrangements r) records |> List.sortByDescending (trd>>List.length)
    
    // let invalids = List.filter (fun (ix, r, sols) -> List.exists (fun s -> not <|isSolution r s) sols) arrangements  
    // if not <| List.isEmpty invalids then
    //     failwith "invalids"
    arrangements |> List.map (trd>>List.length) |> List.sum
    
    
let day12b rs:int =
    let fiveTimes r =
        {
            Gears = List.repeat 5 r.Gears
            Damages = List.repeat 5 r.Damages            
        }
    let newRecords = List.map fiveTimes rs
    day12a newRecords    