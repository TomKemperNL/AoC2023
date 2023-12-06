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
//
//[<Test>]    
//let day6bTest () =
//    Assert.AreEqual(39570185L, day6a inputB)
//    ()
//    
//(a-b)^2 = a^2 -2ab+b^2
//
//333163512891532 = th * (53837288 - th)
//333163512891532 = 53837288th - th^2
//a = th
//b = 53837288 / 2
//-333163512891532 + 724613394798736 = th^2 - (2 * 26918644*th) + 724613394798736
//
//391.449.881.907.204 = (th - 26918644)^2
//
//??uh oh??
//19.785.092,4159 = th - 26918644
//th1 = 19.785.092,4159 + 26918644 = 46.703.736,41593791
//th2 = -19.785.092,4159 = th - 26918644 = 7.133.551,5841
//th1 - th2 = 39570185