module aoc2023.Day4

open System
open Shared
open aoc2023

type Card = int * Set<int> * Set<int>

let parse (line: string) : Card =
    match line with
    | ParseRegex "Card\s+(\d+): (.+) \| (.+)" [nr; winningStr; nrStr] ->
        
        let parseNrs (txt: string) =
            txt.Trim().Split(" ")
                |> Array.map (fun s -> s.Trim())
                |> Array.filter (fun s -> (not (System.String.IsNullOrWhiteSpace(s))))
//                |> Array.map (fun s -> printfn "%s" s;s)
                |> Array.map int
                |> Set.ofArray
        
        let winningNrs = parseNrs winningStr
        let nrs = parseNrs nrStr
        (int nr, winningNrs, nrs)
    | _ ->
        failwith (sprintf "cannot parse %s" line)
        
let matches (card: Card) : int = 
    let _, winners, numbers = card
    Set.intersect winners numbers |> Set.count
    
let score (card: Card) : int =
    let overlapping = matches card    
    2.0 ** (overlapping - 1 |> float) |> int
    
let day4a (cards: Card list) =
    List.map score cards |> List.sum
    

let rec addCards depth times nrWithCards =
    match depth with
    | 0 -> nrWithCards
    | d ->
        let (nr, card) :: rest = nrWithCards        
        (nr + times, card) :: (addCards (d-1) times rest)

let rec countCards cardsWithNrs =
    match cardsWithNrs with
    | [] -> 0
    | (nr, card) :: rest ->
        let nrOfMatches = (matches card) 
        let newRest = addCards nrOfMatches nr rest
        nr + countCards newRest
    
let day4b (cards: Card list) =
    let bunchOfOnes = List.init (List.length cards) (fun _ -> 1)
    let nrWithCard = List.zip bunchOfOnes cards
    countCards nrWithCard
    
    
    
    
    