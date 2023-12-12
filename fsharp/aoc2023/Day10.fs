module aoc2023.Day10

open Shared

type Direction = N | S | E | W
type Pipe = |NS | EW | NE | NW | SW | SE
module Pipe =
    let allPieces = [
        NS;EW;NE;NW;SW;SE
    ]
    let exits (x,y) pipe =
        match pipe with
        | NS -> [(x, y - 1); (x, y+1)]
        | EW -> [(x-1, y); (x+1), y ]
        | NE -> [(x, y - 1); (x+1, y)]
        | NW -> [(x, y - 1); (x-1, y)]
        | SE -> [(x, y + 1); (x+1, y)]
        | SW -> [(x, y + 1); (x-1, y)]
    
    let connectsTo (x,y) pipe (tx,ty) =
        let exits = (exits (x,y) pipe)
        List.contains (tx,ty) exits
    
    let whatFitsBetween (x,y) (sx, sy) (dx, dy) : Pipe option =
        let optionsSource = List.filter (fun p -> connectsTo (x,y) p (sx,sy)) allPieces
        let optionsTarget = List.filter (fun p -> connectsTo (x,y) p (dx,dy)) allPieces
        let remaining = List.intersect optionsSource optionsTarget
        match remaining with
        | [] -> None
        | [p] -> Some p
        | _ -> failwith "uuuuh, shouldn't be possible"
                
    let private pipeMap = Map.ofList [    
        ('|', NS)
        ('-', EW)
        ('L', NE)
        ('J', NW)
        ('7', SW)
        ('F', SE)        
    ]    
    let private reverseMap = Map.flip pipeMap    
    let ofChar c = Map.find c pipeMap
    let toChar p = Map.find p reverseMap
    

type Tile = Pipe of Pipe | Empty

type ParsingTile =
    | Start
    | Tile of Tile

type ParsedMaze = {
    Maze: ParsingTile[][]
    Start: (int*int)
}

type Maze = {
    Maze : Tile[][]
    Start : (int*int)
}

module Maze =
    let get maze (x,y) =
        maze.Maze[y][x]

let neighbours width height (x,y) =
    [(x-1,y); (x+1,y); (x, y-1); (x, y + 1)]
    |> List.filter (fun (x,y) -> x >= 0 && y >= 0)
    |> List.filter (fun (x,y) -> x < width && y < height)

let replaceStart (parsedMaze: ParsedMaze) : Maze =
    let height = parsedMaze.Maze.Length
    let width = parsedMaze.Maze[0].Length
    
    let grabPipe ((x,y): int*int, t: ParsingTile) : ((int*int)*Pipe) option =
        match t with
        | Tile (Pipe p) -> Some ((x,y),p)
        | _ -> None
    
    let nbs = neighbours width height parsedMaze.Start
              |> List.map (fun (x,y) -> (x,y), parsedMaze.Maze[y][x])
    let nbsPipes = nbs |> List.map grabPipe 
                    |> List.choose id
    let connectingPipes = nbsPipes                
                          |> List.filter (fun ((nbx,nby), p) -> Pipe.connectsTo (nbx, nby) p parsedMaze.Start)
                          |> List.map fst   
              
    match connectingPipes with
    | [a;b] ->
        let startPipe = Pipe.whatFitsBetween parsedMaze.Start a b
        match startPipe with
        | Some p ->
            let toTile (x,y) : Tile =
                match parsedMaze.Maze[y][x] with
                | Tile t -> t
                | Start -> Pipe p
            
            let maze = Array.init height (fun y-> Array.init width (fun x -> toTile (x,y))) 
            
            { Start = parsedMaze.Start
              Maze = maze }
                  
        | None -> failwith "nothing fits?"
    | [] -> failwith "Cannot determine starting piece, no connecting neighbours"
    | [_] -> failwith "Cannot determine starting piece, only one connecting neighbour"
    | _ -> failwith "Cannot determine starting piece, too many connecting neighbours"
            

let parse lines : Maze =
    let parseLine yIx (line: string) : (int*int) option * (ParsingTile list) =        
        let rec parseRec xIx (chars: char list) : ((int*int) option * ParsingTile list)=
            match chars with
            | [] -> None, []
            | h :: t ->
                let (start, tiles) = parseRec (xIx + 1) t
                match h with
                | '.' ->
                    start, Tile Empty :: tiles  
                | 'S' ->
                    Some (xIx, yIx), Start :: tiles 
                | c ->
                    let pipe = Pipe.ofChar c
                    start, Tile (Pipe pipe) :: tiles
                    
        let start, tiles = parseRec 0 (line.Trim().ToCharArray() |> List.ofArray)
        start, tiles
    
    let parsed = List.mapi parseLine lines
    let parsedTiles = List.map snd parsed
    let start = List.pick fst parsed 
    
    replaceStart {
        Start = start
        Maze = List.map List.toArray parsedTiles |> List.toArray
    }

let possibleSteps (map: Maze) (x, y) =
    match map.Maze[y][x] with
    | Empty -> []
    | Pipe p ->
        Pipe.exits (x,y) p

let step (map: Maze) from (x,y) =
    match possibleSteps map (x,y) with
    | [] -> failwith "no possible steps, did we exit the circuit somewhere?"
    | [d] when d = from -> failwith "dead end, not possible?"
    | [a; b] when a = from -> b
    | [a; b] when b = from -> a
    | _ -> failwith "no idea..."

let day10a (map: Maze) =
    
    let possiblePaths = possibleSteps map map.Start
    match possiblePaths with
    | [pathA; pathB] ->
        
        let rec keepStepping steps (fromA, posA: int*int, visitedA, fromB, posB: int*int, visitedB) =
            let newVisitedA = Set.add posA visitedA
            let newVisitedB = Set.add posB visitedB
            
            if visitedA = newVisitedA then failwith "Looping, should always be somewhere new"
            if visitedB = newVisitedB then failwith "Looping, should always be somewhere new"
            
            if Set.contains posB newVisitedA || Set.contains posA newVisitedB then steps
            else
                let newA : int*int = step map fromA posA
                let newB : int*int = step map fromB posB
                keepStepping (1 + steps) (posA, newA, newVisitedA, posB, newB, newVisitedB)
        
        keepStepping 1 (map.Start, pathA, Set.singleton map.Start, map.Start, pathB, Set.singleton map.Start)
    | _ ->
        failwith "????"
    
    
    
    
let day10b = day10a    