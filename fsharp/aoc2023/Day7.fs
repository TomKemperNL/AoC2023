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

let parse line : Bid = 
    match line with 
    | ParseRegex "(.)(.)(.)(.)(.) (\d+)" [a;b;c;d;e;bid] ->
        let hand: Hand = List.map (fun s -> Map.find s cardTranslation) [a;b;c;d;e] |> fun [a;b;c;d;e] -> (a,b,c,d,e)
        let parsedBid = int(bid)
        (hand, parsedBid)
    | _ -> failwith (sprintf "cannot parse %s" line)

let asList ((a,b,c,d,e): Hand) = [a;b;c;d;e]  

let categoriseHand (h: Hand) = 
    let counts = List.groupBy id (asList h) |> List.map (fun (c, cs) -> (c, List.length cs)) |> List.sortByDescending snd
    match counts with
    | [(c, 5)] -> FiveOfAKind
    | [(c, 4); _ ] -> FourOfAKind
    | [(c, 3); (d, 2) ] -> FullHouse
    | (c, 3) :: _ -> ThreeOfAKind
    | [(c, 2); (d, 2); _ ] -> TwoPair
    | (c, 2) :: _ -> OnePair
    | _ -> HighCard
    
let compareHand h1 h2 =
    let compare = Microsoft.FSharp.Core.Operators.compare    
    match compare (categoriseHand h1) (categoriseHand h2) with
    | n when n < 0 -> -1
    | n when n > 0 -> 1
    | 0 ->
        match compare (asList h1) (asList h2) with
        | n when n < 0 -> -1
        | n when n > 0 -> 1
        | 0 -> 0

let day7a (bids: Bid list) =
    List.sortWith (fun (h1, _) (h2, _) -> compareHand h1 h2) bids
    |> List.mapi (fun ix (_, bid) -> (ix+1) * bid)
    |> List.sum
    