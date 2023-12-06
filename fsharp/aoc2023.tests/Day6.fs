module aoc2023.Day6.tests

open System.IO
open NUnit.Framework

let example = [
    { Time = 7; Distance= 9 }
    { Time = 15; Distance= 40 }
    { Time = 30; Distance= 200 }
]   
    
let exampleB = [
    { Time = 71530; Distance= 940200 }
]

let input = [
    { Time = 53; Distance= 333 }
    { Time = 83; Distance= 1635 }
    { Time = 72; Distance= 1289 }
    { Time = 88; Distance= 1532 }
]

let inputB = [
    { Time = 53837288; Distance= 333163512891532L }
]

[<Test>]
let day6aExampleTest () =
    Assert.AreEqual(288, day6a example)

[<Test>]    
let day6aTest () =
    Assert.AreEqual(140220, day6a input)
    ()

[<Test>]
let day6bExampleTest () =
    Assert.AreEqual(71503, (day6a exampleB))
    ()

[<Test>]    
let day6bTest () =
    Assert.AreEqual(39570185L, day6a inputB)
    ()
    
