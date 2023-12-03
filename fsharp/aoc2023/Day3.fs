module aoc2023.Day3

open System.Text.RegularExpressions

type Number =
    { row: int
      startCol: int
      endCol: int
      value: int }

type GearCandidate = { row: int; col: int }

let symbols =
    "*-%/#&$=@+".ToCharArray() |> List.ofArray

let getCharAt (map: string []) (col, row) : char =
    let line = map.[row]
    line.Chars col

let findNumbers row line : Number list =
    let matches = Regex.Matches(line, "\d+")

    let matchToNr (m: Match) =
        { row = row
          startCol = m.Index
          endCol = m.Index + m.Length - 1
          value = int m.Value }

    Seq.map matchToNr matches |> Seq.toList

let isInNumber (number: Number) (col, row) =
    row = number.row
    && col >= number.startCol
    && col <= number.endCol

let findGears row line : GearCandidate list =
    let matches = Regex.Matches(line, "\*")
    let matchToGear (m: Match) : GearCandidate = { row = row; col = m.Index }
    Seq.map matchToGear matches |> Seq.toList

let isAdjacentToSymbol (lines: string []) (number: Number) : bool =
    let neighbours =
        seq {
            for r in (max 0 (number.row - 1)) .. (min (number.row + 1) (lines.Length - 1)) do
                for c in (max 0 (number.startCol - 1)) .. (min (number.endCol + 1) (lines.[r].Length - 1)) do
                    yield (c, r)
        }

    let chars =
        Seq.map (getCharAt lines) neighbours

    Seq.exists (fun c -> List.contains c symbols) chars

let day3a (lines: string []) =
    let nrs: Number list =
        Array.mapi findNumbers lines
        |> List.ofArray
        |> (List.collect id)

    List.filter (isAdjacentToSymbol lines) nrs
    |> List.map (fun n -> n.value)
    |> List.sum

let getAdjacentNrs (lines: string []) (nrs: Number list) (gear: GearCandidate) : Number list =
    let neighbours =
        seq {
            for r in (max 0 (gear.row - 1)) .. (min (gear.row + 1) (lines.Length - 1)) do
                for c in (max 0 (gear.col - 1)) .. (min (gear.col + 1) (lines.[r].Length - 1)) do
                    yield (c, r)
        }

    let adjacentNrs =
        List.filter (fun n -> Seq.exists (isInNumber n) neighbours) nrs

    adjacentNrs

let day3b (lines: string []) =
    let nrs =
        Array.mapi findNumbers lines
        |> List.ofArray
        |> (List.collect id)

    let gears =
        Array.mapi findGears lines
        |> List.ofArray
        |> (List.collect id)

    List.map (getAdjacentNrs lines nrs) gears
    |> List.filter (fun nrs -> List.length nrs = 2)
    |> List.map (fun [ x; y ] -> x.value * y.value)
    |> List.sum
