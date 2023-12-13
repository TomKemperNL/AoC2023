module aoc2023.Day12.tests


open System.IO
open NUnit.Framework

let example = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45""".Split("\n") |> List.ofArray

let input = File.ReadLines "./Day9.txt" |> List.ofSeq

[<Test>]
let day12aExampleTest () =
    Assert.AreEqual(114, day12a (parse example))

[<Test>]
let day12aTest () =
    Assert.AreEqual(1772145754L, day12a (parse input))
    
[<Test>]
let day12bExampleTest () =
    Assert.AreEqual(2, day12b (parse example))
    

[<Test>]
let day12bTest () =
    Assert.AreEqual(867, day12b (parse input))    