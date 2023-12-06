module aoc2023.Day5.tests

open System.IO
open NUnit.Framework

let example = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""      .Split("\n") |> Array.toList

[<Test>]
let day5AExample () =    
    Assert.AreEqual(35, day5a (parse example) )
    ()

let input =
    File.ReadLines "./Day5.txt" |> Seq.toList

[<Test>]
let day5A () =    
    Assert.AreEqual(3374647L, day5a (parse input) )
    ()

    
[<Test>]
let day5BExample () =    
    Assert.AreEqual(46, day5b (parse example) )
    
[<Test>]
let processRangeMapping () =
    let rangeMapping = {
        SourceRangeStart = 10
        DestinationRangeStart = 20
        RangeLength = 5
    }
    let result = processRange rangeMapping (2,2)    
    Assert.AreEqual([(2L,2L)], result)
    
    let result = processRange rangeMapping (20, 2)
    Assert.AreEqual([(20L,2L)], result)        
    
    let result = processRange rangeMapping (8, 5)
    Assert.AreEqual([(8L,2L); (20L,3L)], result)    
    
    let result = processRange rangeMapping (13, 2)
    Assert.AreEqual([(23L,2L)], result)
    
    let result = processRange rangeMapping (10, 5)
    Assert.AreEqual([(20L,5L)], result)    
    
    let result = processRange rangeMapping (13, 7)
    Assert.AreEqual([(23L,2L); (15L,5L)], result)   
    ()

[<Test>]
let day5B () =    
    Assert.AreNotEqual(day5b (parse input) , 51914834L)