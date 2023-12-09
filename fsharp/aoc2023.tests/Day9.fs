module aoc2023.Day9.tests

open System.IO
open NUnit.Framework

let example = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45""".Split("\n") |> List.ofArray

let input = File.ReadLines "./Day9.txt" |> List.ofSeq

[<Test>]
let day9aExampleTest () =
    Assert.AreEqual(114, day9a (parse example))

[<Test>]
let day9aTest () =
    Assert.AreEqual(1772145754L, day9a (parse input))    