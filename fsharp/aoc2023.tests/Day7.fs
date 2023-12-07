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
let compareHands () =
    Assert.AreEqual(1, compareHand (Five, Five, Five, Five, Five) (Five, Five, Four, Five, Five))
    Assert.AreEqual(1, compareHand (Five, Five, Three, Three, Three) (Five, Five, Two, Two, Two))
    Assert.AreEqual(-1, compareHand (Two, Three, Four, Five, Six) (Six, Five, Four, Three, Two))
    
    Assert.AreEqual(1, compareHand (Two, Two, Two, Five, Six) (Six, Six, Four, Four, Two))
    

[<Test>]
let day7aExampleTest () =
    Assert.AreEqual(6440, day7a (List.map parse example))

[<Test>]    
let day7a () =
    Assert.AreEqual(248179786, day7a (List.map parse input))

[<Test>]
let categoriseHandsWithJokers () =
    Assert.AreEqual(FiveOfAKind, categoriseHand (Five, Five, Five, Five, Joker))
    Assert.AreEqual(FourOfAKind, categoriseHand (Five, Five, Four, Joker, Joker))
    Assert.AreEqual(FullHouse, categoriseHand (King, King, Joker, Four, Four))
    

[<Test>]
let day7bExampleTest () =
    Assert.AreEqual(5905, day7b (List.map parseB example))

[<Test>]    
let day7b () =
    Assert.AreEqual(247885995, day7b (List.map parseB input))    