module aoc2023.Day10.tests

open System.IO
open NUnit.Framework

let example1 = """.....
.S-7.
.|.|.
.L-J.
.....
"""             .Split("\n") |> List.ofArray

let example2 = """..F7.
.FJ|.
SJ.L7
|F--J
LJ..."""             .Split("\n") |> List.ofArray

let input = File.ReadLines "./Day10.txt" |> List.ofSeq

[<Test>]
let day10aExample1Test () =
    Assert.AreEqual(4, day10a (parse example1))
    

[<Test>]
let day10aExample2Test () =
    Assert.AreEqual(8, day10a (parse example2))    

[<Test>]
let day10aTest () =
    Assert.AreEqual(0, day10a (parse input))
    