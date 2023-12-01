module aoc2023.Day1

let numbers =
    List.map (fun n -> (string(n), n)) [1..9]  

let words =
    [ ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9) ]

type ReadingDirection =
    | LeftToRight
    | RightToLeft

let findNr candidates dir word =
    let rec findRec gathered toScan =
        match toScan with
        | "" -> 0
        | _ ->
            let next = toScan.Substring(0, 1)
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

    if dir = LeftToRight then
        findRec "" word
    else
        let reverted = word.ToCharArray() |> Array.rev |> System.String
        findRec "" reverted

let findNrs candidates (word: string) =
    (findNr candidates LeftToRight word, findNr candidates RightToLeft word)

let toNr (left, right) = 10 * left + right

let day1 lines =
    List.map (findNrs (Map.ofList numbers) >> toNr) lines
    |> List.sum

let day1b lines =
    let targets = Map.ofList (List.concat [numbers; words])    
    let nrs =
        List.map (findNrs targets >> toNr) lines
    List.sum nrs