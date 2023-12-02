module aoc2023.Day2.tests

open System.IO
open NUnit.Framework


let exampleInput =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
        .Split("\n")
    |> List.ofArray

let input =
    File.ReadLines "./Day2.txt" |> List.ofSeq

[<Test>]
let canParseInput () =
    Assert.AreEqual((Red, 5), parseColour "5 red")
    Assert.AreEqual((Green, 15), parseColour "15 green")
    Assert.AreEqual((Blue, 42), parseColour "42 blue")
    
    let parsed = List.map parse exampleInput |> List.choose id
    
    Assert.AreEqual(5, List.length parsed)
    ()

[<Test>]
let day2AExample () =
    let games = List.map parse exampleInput |> List.choose id
    let bag = { red = 12; green = 13; blue = 14 }
    
    Assert.AreEqual(true, bagAcceptsGame bag (List.item 0 games))
    Assert.AreEqual(true, bagAcceptsGame bag (List.item 1 games))
    Assert.AreEqual(false, bagAcceptsGame bag (List.item 2 games))
    Assert.AreEqual(false, bagAcceptsGame bag (List.item 3 games))
    Assert.AreEqual(true, bagAcceptsGame bag (List.item 4 games))
    
    Assert.AreEqual(8, day2a games)   
    

[<Test>]
let day2A () =
    let games = List.map parse input |> List.choose id
    Assert.AreEqual(3035, day2a games)
    

[<Test>]
let day2BExample () =    
   let games = List.map parse exampleInput |> List.choose id
   Assert.AreEqual(2286, day2b games)
   

[<Test>]
let day2B () =
    let games = List.map parse input |> List.choose id
    Assert.AreEqual(66027, day2b games)   