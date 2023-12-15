module aoc2023.Day14.tests

open System.IO
open NUnit.Framework

let example = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""   .Split("\n")

[<Test>]
let dropTests () =
    let result = drop [Empty;Round;Empty;Round]
    Assert.AreEqual([Round; Round; Empty; Empty], result)
    
    let result = drop [Empty;Round;Square;Empty;Round]
    Assert.AreEqual([Round; Empty; Square; Round; Empty], result)
    
[<Test>]
let weightTests () =
    Assert.AreEqual(7, totalWeight [Round; Round; Empty; Empty])

let input = File.ReadLines "./Day14.txt" |> Array.ofSeq

[<Test>]
let day14aExampleTest () =
    Assert.AreEqual(136, day14a (parse example))

[<Test>]
let day14aTest () =
    Assert.AreEqual(109833, day14a (parse input))
    