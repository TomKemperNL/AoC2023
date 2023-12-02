module aoc2023.Day2
open Shared

type Color = | Red | Green | Blue



type Pull = {
    red: int; green: int; blue: int;    
}

type Bag = {
    red: int; green: int; blue: int;
}

type Game = int * Pull list

let parseColour (colour: string) =
    match colour with
        | ParseRegex "(\\d+) blue" [d] -> (Blue,(int d))
        | ParseRegex "(\\d+) red" [d] -> (Red,(int d))
        | ParseRegex "(\\d+) green" [d] -> (Green,(int d))
        | _ -> failwith (sprintf "cannot parse %s" colour)

let parsePull (pull: string) : Pull =
    let parts = pull.Split(",")
    let colours = Seq.map parseColour parts |> Map.ofSeq
    {
        red = Map.findOrElse 0 Red colours
        green = Map.findOrElse 0 Green colours
        blue = Map.findOrElse 0 Blue colours        
    }
        
let parse line : Game option =
    match line with
    | ParseRegex "Game (\\d+): (.+)" [id; gamesLine] ->
        let id = int id
        let pulls = gamesLine.Split(";") |> Seq.map parsePull |> Seq.toList
        Some (id, pulls)       
    | _ ->
        None

let bagAcceptsPull (bag: Bag) (pull: Pull) =
    bag.blue >= pull.blue && bag.green >= pull.green && bag.red >= pull.red
    
let bagAcceptsGame (bag: Bag) (game: Game) =
    let _, pulls = game
    List.forall (bagAcceptsPull bag) pulls
    
let power (bag: Bag) : int =
    bag.blue * bag.green * bag.red

let minPull (pullA: Bag) (pullB: Pull) =
    {
        red = if pullB.red > pullA.red then pullB.red else pullA.red
        green = if pullB.green > pullA.green then pullB.green else pullA.green
        blue = if pullB.blue > pullA.blue then pullB.blue else pullA.blue
    }
    
let minBag (game: Game) : Bag = 
    let _, pulls = game
    List.fold minPull { red=0;green=0;blue=0; } pulls

let day2a (games: Game list) =
    let bag = { red = 12; green = 13; blue = 14 }
    
    List.filter (bagAcceptsGame bag) games
        |> List.map fst
        |> List.sum
        
let day2b (games: Game list) =    
    List.map minBag games
    |> List.map power
    |> List.sum