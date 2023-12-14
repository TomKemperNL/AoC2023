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
    Assert.AreEqual(2, generatePossibleSolutionsBackfill true 1 [Unknown; Unknown] |> List.length)
    Assert.AreEqual(0, generatePossibleSolutionsBackfill true 1 [Operational; Operational] |> List.length)
    Assert.AreEqual(1, generatePossibleSolutionsBackfill true 1 [Damaged; Operational] |> List.length)
    
    Assert.AreEqual(1, generatePossibleSolutionsBackfill true 3 (List.head records).Gears |> List.length)
    Assert.AreEqual(0, generatePossibleSolutionsBackfill true 2 (List.head records).Gears |> List.length)
    Assert.AreEqual(0, generatePossibleSolutionsBackfill true 4 (List.head records).Gears |> List.length)



[<Test>]
let testExampleLine1 () =
    let records = parse example
    
    // Assert.AreEqual(1,  arrangements (List.item 0 records) |> List.length)
    Assert.AreEqual("#.#.###",  arrangements (List.item 0 records) |> List.map Gear.toString |> List.head)

[<Test>]
let testExampleLine2 () =
     let records = parse example
     let ex2 = List.item 1 records
     let possibilities = generatePossibleSolutionsBackfill true 3 (Gear.ofString ".??..??...?##.")
     Assert.AreEqual(1, List.length possibilities)
     let result = List.head possibilities
     Assert.AreEqual(".??..??...###.", Gear.toString result)

[<Test>]
let day12aPermutationTest () =
    let records = parse example
    
    // Assert.AreEqual(1,  arrangements (List.item 0 records) |> List.length)
    let ar2 = arrangements (List.item 1 records)
    Assert.AreEqual(4,  ar2 |> List.length)
    Assert.AreEqual(1,  arrangements (List.item 2 records) |> List.length)
    Assert.AreEqual(1,  arrangements (List.item 3 records) |> List.length)
    Assert.AreEqual(4,  arrangements (List.item 4 records) |> List.length)
    Assert.AreEqual(10,  arrangements (List.item 5 records) |> List.length)
    

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