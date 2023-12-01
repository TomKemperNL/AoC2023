﻿module aoc2023.Day1.tests

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