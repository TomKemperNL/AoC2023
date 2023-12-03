module aoc2023.Day3.tests

open System.IO
open NUnit.Framework

let exampleInput = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""       .Split("\n") //Eeeeuh, you ok F#?

let input =
    File.ReadLines "./Day3.txt" |> Array.ofSeq

[<Test>]
let day3aExample () =
    Assert.AreEqual(4361, day3a exampleInput)
    ()

[<Test>]
let day3a () =
    Assert.AreEqual(532428, day3a input)
    ()    

[<Test>]
let day3bExample () =
    Assert.AreEqual(467835, day3b exampleInput)
    ()
    
[<Test>]
let day3b () =
    Assert.AreEqual(84051670, day3b input)
    ()
    
//let chars = Array.map (fun (s: string) -> s.ToCharArray()) input
//let symbols  = Array.collect id chars |> Array.distinct
//
//[<Test>]
//let distinctChars () =
//    printfn "%A" symbols
//    Assert.AreEqual([||], symbols)
//    ()
                                               
                                               