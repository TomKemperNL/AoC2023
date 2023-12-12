module aoc2023.Day11

open System.Collections.Generic
open Microsoft.FSharp.Core

type Coordinate = int*int
type Pair = Coordinate * Coordinate
type Pixel =
    | Empty
    | Galaxy

module Pixel =
    let private mapping =
        Map.ofList [ ('#', Galaxy)
                     ('.', Empty) ]
    let ofChar c : Pixel option = Map.tryFind c mapping
    let toChar p : char =
        match p with
        | Galaxy -> '#'
        | Empty -> '.'

type Image = {
    Pixels : Pixel[][]
    EmptyRows : int list
    EmptyColumns: int list
    Stars: (int*int) list
}

let parse (lines: string list) =
    let height = List.length lines
    let width = (List.head lines).Length
    
    let possiblyEmptyColumns = List<System.Int32>(Seq.ofList [0..width - 1])
    let possiblyEmptyRows = List<System.Int32>(Seq.ofList [0..height - 1])
    let stars = List<int*int>()
    
    let rec parseLine y (line: string) : Pixel list =
        let chars = line.Trim().ToCharArray() |> List.ofArray        
        let parseChar x c : Pixel =
            match c with
            | '.' -> Empty
            | '#' -> 
                possiblyEmptyColumns.Remove(x) |> ignore
                possiblyEmptyRows.Remove(y) |> ignore
                stars.Add(x,y)
                Galaxy
        
        List.mapi parseChar chars    
    let pixelLines = List.mapi parseLine lines
    
    {
        EmptyRows = List.ofSeq possiblyEmptyRows
        EmptyColumns = List.ofSeq possiblyEmptyColumns
        Stars = List.ofSeq stars
        Pixels = List.map (List.toArray) pixelLines |> List.toArray
    }

let findPairs (stars: Coordinate list) : Pair list =
    let rec findPairsRec strs =
        match strs with
        | [] -> []
        | h :: t ->
            let newPairs = List.map (fun i -> h, i) t
            let rest = findPairsRec t
            List.append newPairs rest
    findPairsRec stars

let distance factor (image:Image) ((x1, y1), (x2,y2)) : int64 =
    let baseDistance = abs (x1 - x2) + abs (y1 - y2)
    let leftX = min x1 x2
    let rightX = max x1 x2
    let lowerY = min y1 y2
    let highY = max y1 y2
    
    let emptyCols = List.filter (fun n -> n >= leftX && n <= rightX) image.EmptyColumns |> List.length
    let emptyRows = List.filter (fun n -> n >= lowerY && n <= highY) image.EmptyRows |> List.length
    
    (int64 baseDistance) + (int64 emptyCols * (factor-1L)) + (int64 emptyRows * (factor-1L))
    
let day11a (image: Image) =
    let pairs = findPairs image.Stars
    let distances = List.map (distance 2 image) pairs 
    List.sum distances
    
let day11b factor image =
    let pairs = findPairs image.Stars
    let distances = List.map (distance factor image) pairs 
    List.sum distances