module aoc2023.Day7.tests

open System.IO
open NUnit.Framework


let example = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""    .Split("\n") |> List.ofArray

let input =
    File.ReadLines "./Day7.txt" |> List.ofSeq
    

[<Test>]
let categoriseHands () =
    Assert.AreEqual(FiveOfAKind, categoriseHand (Five, Five, Five, Five, Five))
    Assert.AreEqual(FourOfAKind, categoriseHand (Five, Five, Four, Five, Five))
    Assert.AreEqual(FullHouse, categoriseHand (King, King, Four, Four, Four))
    Assert.AreEqual(ThreeOfAKind, categoriseHand (King, Ace, Four, Four, Four))
    Assert.AreEqual(TwoPair, categoriseHand (King, King, Four, Four, Ace))
    Assert.AreEqual(OnePair, categoriseHand (King, King, Four, Jack, Ace))
    Assert.AreEqual(HighCard, categoriseHand (King, Three, Four, Jack, Ace))

    

[<Test>]
let day7aExampleTest () =
    Assert.AreEqual(6440, day7a (List.map parse example))

[<Test>]    
let day7a () =
    Assert.AreEqual(0, day7a (List.map parse input))
    
    