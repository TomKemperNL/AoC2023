module aoc2023.Day14

type Rock =
    | Round
    | Square
    | Empty 

let parse lines =
     let rec parseLine y (line: string) : Rock array =
        let chars = line.Trim().ToCharArray()         
        let parseChar x c : Rock =
            match c with
            | 'O' -> Round
            | '#' -> Square
            | '.' -> Empty                
        Array.mapi parseChar chars
     Array.mapi parseLine lines

let drop column =    
    let rec dropRec col spaceToDrop =
        match col with 
        | [] -> spaceToDrop
        | Empty :: rest -> dropRec rest (Empty::spaceToDrop)
        | Round :: rest ->            
            Round :: (dropRec rest spaceToDrop)
        | Square :: rest ->
            List.concat [spaceToDrop;[Square];dropRec rest []] 
            
    dropRec column []

let column (panel: Rock[][]) n =
    seq {
        for y in 0..panel.Length - 1 do
            yield panel[y][n]                
    } |> Seq.toList
    
let totalWeight column =
    let lngth = List.length column
    let weight ix rock =
        match rock with
        | Round -> lngth - ix
        | _ -> 0
    Seq.mapi weight column |> Seq.sum
    
    
let day14a (panel: Rock[][]) : int =
    let width = panel[0].Length
    [0..width-1]
        |> List.map (column panel)
        |> List.map drop 
        |> List.map totalWeight
        |> List.sum