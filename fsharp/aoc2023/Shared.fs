﻿module aoc2023.Shared

open System
open System.Text.RegularExpressions

// ParseRegex parses a regular expression and returns a list of the strings that match each group in
// the regular expression.
// List.tail is called to eliminate the first element in the list, which is the full matched expression,
// since only the matches for each group are wanted.
let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let digits = "123456789".ToCharArray() |> List.ofArray

module Pair =
    let flip (a,b) = (b,a)
    
module Map =
    let findOrElse ifNone key map =
        match Map.tryFind key map with
        | None -> ifNone
        | Some item -> item
    
    let flip<'A, 'B when 'A : comparison and 'B : comparison> (m: Map<'A, 'B>) = Map.toList m |> List.map (Pair.flip) |> Map.ofList

module List =
    let rec replace old replacement lst =
        match lst with
        | [] -> []
        | h :: t when h = old -> replacement :: replace old replacement t
        | h :: t -> h :: replace old replacement t
        
    let rec findIndexes pred lst =
        let rec findRec currentIndex items =
            match items with
            | [] -> []
            | h :: t when pred h ->
                currentIndex :: findRec (currentIndex + 1) t
            | _ :: t ->
                findRec (currentIndex + 1) t
                
        findRec 0 lst
        
