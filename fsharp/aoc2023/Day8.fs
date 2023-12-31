﻿module aoc2023.Day8

open System.Collections.Concurrent
open System.Collections.Generic
open System.Text.RegularExpressions
open Shared

type Move = Left | Right

type Label = (char*char*char)
module Label =   
    
    let ofArray (larray: char array) =
        match larray with 
        | [| a;b;c |] -> (a,b,c)
        | _ -> failwith <| sprintf "Nope %A" larray
        
    let ofString (str: string) =
        ofArray (str.ToCharArray())
        
type Node = Node of Label * Label * Label
module Node =
    let label n =
        match n with
        | Node (l, _, _) -> l

type DesertMap = {
    Moves: Move list
    Nodes: Map<Label, Node>
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
                
                Label.ofString name, Node (Label.ofString name, Label.ofString left, Label.ofString right)
            | _ -> failwith <| sprintf "cannot parse %s" line
            
        {
            Moves = recParseMoves instructionsChars
            Nodes = Map.ofSeq (List.map parseLine map) 
        }   
    | _ -> failwith "cannot parse first 3 lines"


type MemoInput = (Map<Label, Node>*Move list*Label)
let memoMap = ConcurrentDictionary<MemoInput, Label>()

let runMoves map instructions (currentPosition: Label) =
    let key = map, instructions, currentPosition
    if memoMap.ContainsKey(key) then            
        memoMap[key]        
    else 
        let rec runMovesRec map instructions (currentPosition: Label) : Label =
                    match Map.find currentPosition map with
                    | Node (_, left, right) ->     
                        match instructions with
                        | [] -> currentPosition
                        | Left :: t ->        
                            runMovesRec map t left 
                        | Right :: t ->
                            runMovesRec map t right
        let result = runMovesRec map instructions currentPosition
        memoMap.GetOrAdd(key, result)

let day8a (map: DesertMap) =
    let stepsPerRun : int64 = List.length map.Moves
    let rec runUntil (destination: Label) (position: Label) =
        match position with
        | _ when destination = position -> 0L
        | _ ->                       
            let newPos = runMoves map.Nodes map.Moves position
            1L + runUntil destination newPos
    
    let runs = runUntil (Label.ofString "ZZZ") (Label.ofString "AAA")
    runs * stepsPerRun
    

let endsWith char (l: Label) =
    match l with
    | (_,_,c) when char = c -> true
    | _ -> false
        
let day8b (map: DesertMap) =
    let stepsPerRun : int64 = List.length map.Moves
        
    let startingPositions = Seq.filter (endsWith 'A') (Map.keys map.Nodes)
    let endPositions = Seq.filter (endsWith 'Z') (Map.keys map.Nodes)
    let isDone positions =
        Seq.forall (endsWith 'Z') positions
        
    let rec runUntil (positions: Label seq) =
        if isDone positions then 0L
        else
            let newPositions = Seq.map (runMoves map.Nodes map.Moves) positions            
            1L + (runUntil newPositions)
            
    let runs = runUntil startingPositions            
    runs * stepsPerRun
    
    
type SequenceAnalysis = {
    LoopStart: int
    LoopLength: int
    ZsInLoop: int list
}    
let analyseMove (map: DesertMap) startPos : SequenceAnalysis = 
    
    let rec findLoopRec visited currentPos =
        match List.exists (fun l -> currentPos = l) visited with
        | true ->
            let visitedInOrder = List.rev visited
            let startOfLoop = List.findIndex (fun l -> currentPos = l) visitedInOrder
            let loop = List.skip (startOfLoop) visitedInOrder
            
            
            //Loop gevonden
            {
                LoopStart = startOfLoop
                LoopLength = List.length loop
                ZsInLoop = List.findIndexes (endsWith 'Z') loop
            }
        | false ->
            let visited = currentPos :: visited
            let newPos = runMoves map.Nodes map.Moves currentPos
            findLoopRec visited newPos
            
    findLoopRec [] startPos
