module aoc2023.Day8.tests

open System.IO
open NUnit.Framework



let example1 = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""    .Split("\n") |> List.ofArray


let example2 = """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""    .Split("\n") |> List.ofArray

let input =
    File.ReadLines "./Day8.txt" |> List.ofSeq


[<Test>]
let day8aExampleTest1 () =
    Assert.AreEqual(2, day8a (parse example1))
    

[<Test>]
let day8aExampleTest2 () =
    Assert.AreEqual(6, day8a (parse example2))
    

[<Test>]
let day8aTest () =
    Assert.AreEqual(16043, day8a (parse input))

let exampleB = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)""".Split("\n") |> List.ofArray


[<Test>]
let day8bExampleTest () =
    Assert.AreEqual(6, day8b (parse exampleB))
    
[<Test>]
let analysiseExampleTest () =
    let map = parse exampleB
    let result1 = analyseMove map ('1','1','A')
    Assert.AreEqual({
        LoopStart = 1
        LoopLength = 1
        ZsInLoop = [0] 
    }, result1)
    let result2 = analyseMove map ('2','2','A')
    Assert.AreEqual({
        LoopStart = 1
        LoopLength = 3
        ZsInLoop = [2] 
    }, result2) 
    ()

[<Test>]
let day8BExlore () =
    let map = parse input
    let startingPositions = Seq.filter (endsWith 'A') (Map.keys map.Nodes)
    let endPositions = Seq.filter (endsWith 'Z') (Map.keys map.Nodes)
    
    let analysis = Seq.map (analyseMove map) startingPositions
    
    //OK dus
    //61 * 43 * 67 * 73 * 79 * 59
// * 263
    //15726453850399
    ()

// [<Test>]
let day8b () =
    Assert.AreEqual(6, day8b (parse input))
    
    