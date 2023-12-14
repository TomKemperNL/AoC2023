module aoc2023.Day12.tests


open System.IO
open NUnit.Framework
open aoc2023.Shared

let example = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1""".Split("\n") |> List.ofArray

let input = File.ReadLines "./Day12.txt" |> List.ofSeq

[<Test>]
let backFillTests () =
    let solutions = generatePossibleSolutionsBackfill 1 (Gear.ofString "??") |> List.map Gear.combine |> List.map Gear.toString
       
    Assert.AreEqual(2,  solutions |> List.length)
    Assert.IsTrue(List.contains ".#" solutions)
    Assert.IsTrue(List.contains "#." solutions)
    
    Assert.AreEqual(0, generatePossibleSolutionsBackfill 1 (Gear.ofString "..") |> List.length)
    Assert.AreEqual(1, generatePossibleSolutionsBackfill 1 (Gear.ofString "#.") |> List.length)   
    


[<Test>]
let testExampleLine1 () =
    let record = parse example |> List.item 0
    let rem, solv = generatePossibleSolutionsBackfill 3 (Gear.ofString "???.###") |> List.map (Pair.map Gear.toString) |> List.head    
    Assert.AreEqual("???.", rem)
    Assert.AreEqual("###", solv)
    
    let solutions = generatePossibleSolutionsBackfill 1 (Gear.ofString "???.") |> List.map (Pair.map Gear.toString)
    Assert.AreEqual(3, List.length solutions)
    Assert.IsTrue(List.contains ("?",".#.") solutions)
    Assert.IsTrue(List.contains ("",".#..") solutions)
    Assert.IsTrue(List.contains ("","#...") solutions)
    
    
    
    
    // Assert.AreEqual(1,  arrangements (List.item 0 records) |> List.length)
    Assert.AreEqual("#.#.###",  arrangements record |> List.map Gear.toString |> List.head)

[<Test>]
let testExampleLine2 () =
     let records = parse example
     let ex2 = List.item 1 records
     let possibilities = generatePossibleSolutionsBackfill 3 (Gear.ofString ".??..??...?##.")
     Assert.AreEqual(1, List.length possibilities)
     let result = List.head possibilities |> Gear.combine
     Assert.AreEqual(".??..??...###.", Gear.toString result)
     

[<Test>]
let testExampleLineX () =
    let solutions = generatePossibleSolutionsBackfill 5 (Gear.ofString "????.######..#####.")
    Assert.AreEqual(("????.######..", "#####."), solutions.Head |> Pair.map Gear.toString)
    let solutions = generatePossibleSolutionsBackfill 6 (Gear.ofString "????.######..")
    Assert.AreEqual(("????.", "######.."), solutions.Head |> Pair.map Gear.toString)
    let solutions = generatePossibleSolutionsBackfill 1 (Gear.ofString "????.") |> List.map (Pair.map Gear.toString)
    Assert.AreEqual(4, List.length solutions)
    Assert.IsTrue(List.contains ("??",".#.") solutions)
    Assert.IsTrue(List.contains ("?",".#..") solutions)
    Assert.IsTrue(List.contains ("",".#...") solutions)
    Assert.IsTrue(List.contains ("","#....") solutions)
    
    let r = parseLine "????.######..#####. 1,6,5"
    let solutions = arrangements r
    Assert.AreEqual(4, List.length solutions)

[<Test>]
let wutPermutationTest () =
    // Assert.AreEqual(1, arrangements (parseLine "? 1") |> List.length)
    // Assert.AreEqual(2, arrangements (parseLine "?? 1") |> List.length)
    Assert.AreEqual(3, arrangements (parseLine "??? 1") |> List.length)
    Assert.AreEqual(4, arrangements (parseLine "???? 1") |> List.length)
    


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
    //8144 too high
    Assert.AreEqual(7732, day12a (parse input))
  
[<Test>]
let uuuurgh ()=
    let record = parseLine "????????????# 1,1,1,1"
    let perms = arrangements record
    List.iter (Gear.toString >> printfn "%s") perms 

[<Test>]
let asfd () =
    let record = parseLine "?.#???????.?????? 3,1,1,2,1"
    let perms = arrangements record
    List.iter (Gear.toString >> printfn "%s") perms 
    
// [<Test>]
// let day12bExampleTest () =
//     Assert.AreEqual(525152L, day12b (parse example))
//     
//
// [<Test>]
// let day12bTest () =
//     Assert.AreEqual(867, day12b (parse input))    