module aoc2023.Day15.tests

open System.IO
open NUnit.Framework

let example = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""

let input = File.ReadAllText "./Day15.txt" 

[<Test>]
let hashTest () =
    Assert.AreEqual(30, hash "rn=1")

[<Test>]
let day15aExampleTest () =
    Assert.AreEqual(1320, day15a (parse example))

[<Test>]
let day15aTest () =
    Assert.AreEqual(509152L, day15a (parse input))
    
[<Test>]    
let processTests () =    
    let boxes = createBoxes()
    processInstruction boxes (parseInstruction "rn=1")
    Assert.AreEqual(boxes[0]|> List.head |> Lens.toString, "rn 1")
    
[<Test>]
let day15bExampleTest () =
    Assert.AreEqual(145, day15b (parse example))

[<Test>]
let day15bTest () =
    Assert.AreEqual(244403L, day15b (parse input))
    