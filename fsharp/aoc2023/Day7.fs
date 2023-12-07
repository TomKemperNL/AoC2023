module aoc2023.Day7

open Microsoft.FSharp.Reflection
open Shared

type Card =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

let cardTranslation = Map.ofList [
    ("2", Two)
    ("3", Three)
    ("4", Four)
    ("5", Five)
    ("6", Six)
    ("7", Seven)
    ("8", Eight)
    ("9", Nine)
    ("T", Ten)
    ("J", Jack)
    ("Q", Queen)
    ("K", King)
    ("A", Ace)    
    ]

type Hand = (Card * Card * Card * Card * Card)

type HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind

type Bid = Hand * int

type Comparison = | Equal | Greater | Lesser
let comparisonToInt comp =
    match comp with
    | Equal -> 0
    | Greater -> 1
    | Lesser -> -1

let parse line : Bid = 
    match line with 
    | ParseRegex "(.)(.)(.)(.)(.) (\d+)" [a;b;c;d;e;bid] ->
        let hand: Hand = List.map (fun s -> Map.find s cardTranslation) [a;b;c;d;e] |> fun [a;b;c;d;e] -> (a,b,c,d,e)
        let parsedBid = int(bid)
        (hand, parsedBid)
    | _ -> failwith (sprintf "cannot parse %s" line)

let categoriseHand ((a,b,c,d,e): Hand) =    
    let asList = [a;b;c;d;e]  
    let counts = List.groupBy id asList |> List.map (fun (c, cs) -> (c, List.length cs)) |> List.sortByDescending snd
    match counts with
    | [(c, 5)] -> FiveOfAKind
    | [(c, 4); _ ] -> FourOfAKind
    | [(c, 3); (d, 2) ] -> FullHouse
    | (c, 3) :: _ -> ThreeOfAKind
    | [(c, 2); (d, 2); _ ] -> TwoPair
    | (c, 2) :: _ -> OnePair
    | _ -> HighCard
    

let compareHand h1 h2 =
    0

let day7a (hands: Bid list) =
    
    42