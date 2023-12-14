module aoc2023.Day12.tests


open System.IO
open NUnit.Framework

let example = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1""".Split("\n") |> List.ofArray

let input = File.ReadLines "./Day12.txt" |> List.ofSeq

[<Test>]
let backFillTests () =
    let records = parse example
    Assert.AreEqual(2, generatePossibleSolutionsBackfill 1 [Unknown; Unknown] |> List.length)
    Assert.AreEqual(0, generatePossibleSolutionsBackfill 1 [Gear Operational; Gear Operational] |> List.length)
    Assert.AreEqual(1, generatePossibleSolutionsBackfill 1 [Gear Damaged; Gear Operational] |> List.length)
    
    Assert.AreEqual(1, generatePossibleSolutionsBackfill 3 (List.head records).Gears |> List.length)
    Assert.AreEqual(0, generatePossibleSolutionsBackfill 2 (List.head records).Gears |> List.length)
    Assert.AreEqual(0, generatePossibleSolutionsBackfill 4 (List.head records).Gears |> List.length)

[<Test>]
let day12aPermutationTest () =
    let records = parse example
    let solves = arrangements (List.head records)
    Assert.AreEqual(1,  solves |> List.length)
    

[<Test>]
let day12aExampleTest () =
    Assert.AreEqual(21, day12a (parse example))

[<Test>]
let day12aTest () =
    Assert.AreEqual(0, day12a (parse input))
    
    
//     
// [<Test>]
// let day12bExampleTest () =
//     Assert.AreEqual(2, day12b (parse example))
//     
//
// [<Test>]
// let day12bTest () =
//     Assert.AreEqual(867, day12b (parse input))    