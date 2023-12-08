module aoc2023.Day8

open Shared

type Move = Left | Right
type Node = Node of string * string * string

type DesertMap = {
    Moves: Move list
    Nodes: Map<string, Node>
}

let parse (input: string list) : DesertMap =
    match input with
    | instructions :: _ :: map ->
        let instructionsChars = instructions.Trim().ToCharArray() |> List.ofArray
        
        let rec recParseMoves instructions =
            match instructions with
            | [] -> []
            | h :: t ->
                let m = match h with
                        | 'L' -> Left
                        | 'R' -> Right
                m :: recParseMoves t           
        
        let parseLine line =
            match line with
            | ParseRegex "(...) = \((...), (...)\)" [name; left;right] ->
                name, Node (name, left,right)
            | _ -> failwith <| sprintf "cannot parse %s" line
            
        {
            Moves = recParseMoves instructionsChars
            Nodes = Map.ofSeq (List.map parseLine map) 
        }   
    | _ -> failwith "cannot parse first 3 lines"

let rec runMoves map (currentPosition: string) instructions : string =
    match Map.find currentPosition map with
    | Node (_, left, right) ->     
        match instructions with
        | [] -> currentPosition
        | Left :: t ->        
            runMoves map left t
        | Right :: t ->
            runMoves map right t
            
        
    

let day8a (map: DesertMap) =
    let stepsPerRun = List.length map.Moves
    let rec runUntil (destination: string) (position: string) =
        match position with
        | _ when destination = position -> 0
        | _ ->                       
            let newPos = runMoves map.Nodes position map.Moves
            1 + runUntil destination newPos
    
    let runs = runUntil "ZZZ" "AAA"
    runs * stepsPerRun
    
let day8b = day8a