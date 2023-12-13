module aoc2023.Day13.tests


open System.IO
open NUnit.Framework

let example = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45""".Split("\n") |> List.ofArray

let input = File.ReadLines "./Day9.txt" |> List.ofSeq

[<Test>]
let day13aExampleTest () =
    Assert.AreEqual(114, day13a (parse example))

[<Test>]
let day13aTest () =
    Assert.AreEqual(1772145754L, day13a (parse input))
    
[<Test>]
let day13bExampleTest () =
    Assert.AreEqual(2, day13b (parse example))
    

[<Test>]
let day13bTest () =
    Assert.AreEqual(867, day13b (parse input))    