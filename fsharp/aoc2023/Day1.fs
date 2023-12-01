module aoc2023.Day1

open System.Text.RegularExpressions
open Shared


let replacements =
    [ ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9) ]

type Direction =
    | LeftToRight
    | RightToLeft

let findNr candidates dir word =
    let rec findRec gathered toScan =
        match toScan with
        | "" -> 0
        | _ ->
            let next = toScan.Substring(0, 1)

            match next with
            | ParseRegex "(\\d)" [ d ] -> int d
            | _ ->
                let rest = toScan.Substring(1)

                let gatheredNew =
                    match dir with
                    | LeftToRight -> gathered + next
                    | RightToLeft -> next + gathered

                let found =
                    Seq.tryFind (fun (c: string) -> gatheredNew.Contains(c)) (Map.keys candidates)

                match found with
                | Some c -> Map.find c candidates
                | None -> findRec gatheredNew rest

    findRec "" word

let findNrs candidates (word: string) =
    let reverted =
        word.ToCharArray() |> Array.rev |> System.String

    (findNr candidates LeftToRight word, findNr candidates RightToLeft reverted)

let toNr (left, right) = 10 * left + right

let day1 lines =
    List.map (findNrs Map.empty >> toNr) lines
    |> List.sum

let day1b lines =
    let nrs =
        List.map (findNrs (Map.ofList replacements) >> toNr) lines

    printfn "%A" nrs

    let pluslog total nr =
        printfn "Total: %d" (total + nr)
        total + nr

    List.fold pluslog 0 nrs
