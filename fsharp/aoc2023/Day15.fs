module aoc2023.Day15
open Shared

let parse (line: string) =
    line.Split(",") |> Array.toList


let hash (str: string) =
    let hashNext (cur: int) (c: char) =
        let asciiCode = int c
        let cur = cur + asciiCode
        let cur = cur * 17
        let cur = cur % 256
        cur
    Array.fold hashNext 0 (str.ToCharArray())
    
let day15a (input: string list) =
    List.map hash input |> List.sum

[<Struct>]    
type Label = Label of string

[<Struct>]
type Lens = Lens of Label * int

module Lens =
    let strength (l: Lens) : int =
        match l with
        | Lens (_, str) -> str

    let toString (l : Lens) : string =
        match l with
        | Lens (Label label, str) -> $"%s{label} %d{str}"
        
    
type Instruction =
    | Remove of int*Label
    | Replace of int*Lens
    
let parseInstruction str : Instruction =
    match str with
    | ParseRegex "(.+)=(\d+)" [label; strength] ->
        let lens = Lens (Label label, (int strength)) 
        Replace ((hash label), lens)
    | ParseRegex "(.+)-" [label] ->
        Remove ((hash label),(Label label))

let rec remove (label: Label) (box: Lens list) : Lens list =
    match box with
    | [] -> []
    | Lens (l, _) :: t when l = label ->
        t
    | h :: t ->
        h:: remove label t
        
let rec replace (newLens: Lens) (box: Lens list) : Lens list =
    match newLens with
    Lens (label, strength) -> 
        match box with
        | [] -> [newLens]
        | Lens (l, _) :: t when l = label ->
            newLens :: t
        | h :: t ->
            h:: replace newLens t
        
     
let processInstruction (boxes: Lens list array) instruction =
    match instruction with
    | Remove (i, label) ->
        let foundBox = boxes[i]
        let removed = remove label foundBox
        boxes.[i] <- removed
    | Replace(i, lens) ->
        let foundBox = boxes[i]
        let replaced = replace lens foundBox
        boxes.[i] <- replaced

let createBoxes () = Array.init 256 (fun _ -> List<Lens>.Empty) 

let day15b (input: string list) =
    let boxes = createBoxes()
    let instructions = List.map parseInstruction input
    for inst in instructions do
        processInstruction boxes inst
        
    let sumBox boxnr (box: Lens list) =
        List.mapi (fun boxpos lens -> (boxnr+1) * (boxpos+1) * Lens.strength lens) box |> List.sum
    
    Array.mapi sumBox boxes |> Array.sum