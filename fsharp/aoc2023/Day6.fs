module aoc2023.Day6

open System.Collections.Generic

type Race = {
    Time : int64
    Distance: int64 
}

let determineDistance maxTime timeHeld =
    timeHeld * (maxTime - timeHeld)

let raceOptions maxTime =
    let options = [0L .. maxTime]
    List.map (determineDistance maxTime) options

let winningOptions race =
    List.filter (fun n -> n > race.Distance) (raceOptions race.Time)

let day6a (input: Race list) =
    let nrOfOptions race =
        List.length (winningOptions race)
    
    let optionsPerRace = List.map nrOfOptions input
    List.fold (*) 1 optionsPerRace

let day6b input =
    42    