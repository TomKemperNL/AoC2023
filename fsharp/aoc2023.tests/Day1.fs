module aoc2023.Day1.tests

open System.IO
open NUnit.Framework

[<SetUp>]
let Setup () = ()


let example1 = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ]

[<Test>]
let day1Example () =
    Assert.AreEqual(142, day1 example1)
    ()


let input = File.ReadLines "./Day1.txt" |> List.ofSeq

[<Test>]
let day1A () =
    Assert.AreEqual(42, day1 input)
    ()

let example2 = [
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
]    
    
[<Test>]
let trickyPart () =
    Assert.AreEqual("8wo3", replaceNrs "eightwothree")
    ()    
    
[<Test>]
let day1WordsExample () =
    Assert.AreEqual(281, day1b example2)
    ()