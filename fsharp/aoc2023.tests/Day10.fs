module aoc2023.Day10.tests

open System.IO
open NUnit.Framework

let example1 = """.....
.S-7.
.|.|.
.L-J.
....."""             .Split("\n") |> List.ofArray

let example2 = """..F7.
.FJ|.
SJ.L7
|F--J
LJ..."""             .Split("\n") |> List.ofArray

let input = File.ReadLines "./Day10.txt" |> List.ofSeq

[<Test>]
let parseTests () =
    let ex1 = parse example1    
    Assert.AreEqual(Pipe SE, Maze.get ex1 ex1.Start)
    let ex2 = parse example2
    Assert.AreEqual(Pipe SE, Maze.get ex2 ex2.Start)

[<Test>]
let connectsToTests () =
    let start = (0,2)
    let right = (1,2)
    let down = (0,3)
    
    Assert.AreEqual(true, Pipe.connectsTo right NW start)
    Assert.AreEqual(true, Pipe.connectsTo down NS start)
    

[<Test>]
let watFitsBetweenTests () =
    Assert.AreEqual(Some EW, Pipe.whatFitsBetween (0,0) (-1, 0) (1, 0))
    Assert.AreEqual(Some EW, Pipe.whatFitsBetween (0,0) (1, 0) (-1, 0))
    
    Assert.AreEqual(Some SE, Pipe.whatFitsBetween (1,1) (1,2) (2,1))
    Assert.AreEqual(Some SE, Pipe.whatFitsBetween (1,1) (2,1) (1,2))

[<Test>]
let day10aExample1Test () =
    Assert.AreEqual(4, day10a (parse example1))
    

[<Test>]
let day10aExample2Test () =
    Assert.AreEqual(8, day10a (parse example2))    

[<Test>]
let day10aTest () =
    Assert.AreEqual(0, day10a (parse input))
    