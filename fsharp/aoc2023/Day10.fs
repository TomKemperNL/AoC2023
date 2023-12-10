module aoc2023.Day10

open Shared

type Direction = | N | S | E | W
type Pipe = (Direction * Direction)
module Pipe =        
    let private pipeMap = Map.ofList [    
        ('|', (N, S))
        ('-', (E, W))
        ('-', (N, E))
        ('-', (N, W))
        ('-', (S, W))
        ('-', (S, E))        
    ]    
    let private reverseMap = Map.flip pipeMap    
    let ofChar c = Map.find c pipeMap
    let toChar p = Map.find p reverseMap

type Tile = Start | Pipe of Pipe | Empty

type Maze = {
    Maze : Tile[][]
    Start : (int*int)
}

let parse lines =
    
    
    [||]
    
let day10a map =
    42
    
let day10b = day10a    