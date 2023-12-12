module aoc2023.Day11.tests

open System.IO
open NUnit.Framework

let example = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""   .Split("\n") |> List.ofArray


let input = File.ReadLines "./Day11.txt" |> List.ofSeq


[<Test>]
let findPairsTest () =
    Assert.AreEqual(36, List.length (findPairs (parse example).Stars))



[<Test>]
let day11aExampleTest () =
    Assert.AreEqual(374, day11a (parse example))

[<Test>]
let day11aTest () =
    Assert.AreEqual(9684228, day11a (parse input))


[<Test>]
let day11bExampleTest () =
    Assert.AreEqual(1030, day11b 10 (parse example))
    Assert.AreEqual(8410, day11b 100 (parse example))

[<Test>]
let day11bTest () =
    Assert.AreEqual(483844716556L, day11b 1000000 (parse input))
