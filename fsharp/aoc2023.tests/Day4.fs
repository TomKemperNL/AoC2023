module aoc2023.Day4.tests

open System.IO
open NUnit.Framework

let example = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".Split("\n")

let input =
    File.ReadLines "./Day4.txt" |> Array.ofSeq

[<Test>]
let day4aExampleTest () =
    Assert.AreEqual(13, day4a (Array.map parse example |> List.ofArray))

[<Test>]    
let day4a () =
    Assert.AreEqual(28538, day4a (Array.map parse input |> List.ofArray))
    

[<Test>]
let day4bExampleTest () =
    Assert.AreEqual(30, day4b (Array.map parse example |> List.ofArray))

[<Test>]    
let day4b () =
    Assert.AreEqual(9425061, day4b (Array.map parse input |> List.ofArray))
        