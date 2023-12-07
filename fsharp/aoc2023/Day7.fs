module aoc2023.Day7

open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Reflection
open Shared

type Card =
    | Joker
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

let cardTranslationB = Map.add "J" Joker cardTranslation

let allCards = (Map.values cardTranslation |> List.ofSeq)

type Hand = (Card * Card * Card * Card * Card)

type HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind

type Bid = Hand * int

let parseWithMap cardTranslation line : Bid = 
    match line with 
    | ParseRegex "(.)(.)(.)(.)(.) (\d+)" [a;b;c;d;e;bid] ->
        let hand: Hand = List.map (fun s -> Map.find s cardTranslation) [a;b;c;d;e] |> fun [a;b;c;d;e] -> (a,b,c,d,e)
        let parsedBid = int(bid)
        (hand, parsedBid)
    | _ -> failwith (sprintf "cannot parse %s" line)


let parse = parseWithMap cardTranslation
let parseB = parseWithMap cardTranslationB

let asList ((a,b,c,d,e): Hand) = [a;b;c;d;e]

let fromList lst : Hand =
    match lst with
    | [a;b;c;d;e] -> (a,b,c,d,e)
    | _ -> failwith "boom!"
    

let rec categoriseHand (h: Hand) = 
    let counts = List.groupBy id (asList h) |> List.map (fun (c, cs) -> (c, List.length cs)) |> List.sortByDescending snd
    let jokerCounts = List.tryFind (fst >> (=) Joker) counts  |> Option.defaultValue (Joker, 0) |> snd
    match jokerCounts with
    | 0 -> 
        match counts with
        | [(c, 5)] -> FiveOfAKind
        | [(c, 4); _ ] -> FourOfAKind
        | [(c, 3); (d, 2) ] -> FullHouse
        | (c, 3) :: _ -> ThreeOfAKind
        | [(c, 2); (d, 2); _ ] -> TwoPair
        | (c, 2) :: _ -> OnePair
        | _ -> HighCard
    | 5 -> FiveOfAKind
    | 4 -> FiveOfAKind
    | 3 ->
        match counts with
        | [(Joker, 3); (c, 2)] -> FiveOfAKind
        | _ -> FourOfAKind
    | 2 ->
        //Pure gok
        let handLst = asList h
        let candidates = List.map (fun c -> List.replace Joker c  handLst) allCards |> List.map fromList  
        List.sortWith compareHand candidates |> List.rev |> List.head |> categoriseHand
    | 1 ->
        let handLst = asList h
        let candidates = List.map (fun c -> List.replace Joker c  handLst) allCards |> List.map fromList  
        List.sortWith compareHand candidates |> List.rev |> List.head |> categoriseHand  
and
 compareHand h1 h2 =
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
    
let day7b = day7a    