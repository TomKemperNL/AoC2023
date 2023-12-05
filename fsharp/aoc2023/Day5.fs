module aoc2023.Day5

open Microsoft.FSharp.Reflection
open Shared

type StartingSeed = StartingSeed of int64

type ItemKind = //Not necessary, but lets pretend the domain matters...
    | Seed
    | Soil
    | Fertilizer
    | Water
    | Light
    | Temperature
    | Humidity
    | Location

let namesOfKinds =
    let cases =
        FSharpType.GetUnionCases(typeof<ItemKind>)

    let values =
        Array.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> ItemKind) cases

    Array.zip (Array.map (fun (s: UnionCaseInfo) -> s.Name.ToLower()) cases) values
    |> Map.ofArray

type Item = (int64)


type RangeMapping = {
    DestinationRangeStart: int64
    SourceRangeStart: int64
    RangeLength: int64
}

type AlmanacMapping =
    { RangeMapping : RangeMapping
      From: ItemKind
      To: ItemKind }


type Input =
    { Seeds: StartingSeed list
      Mappings: AlmanacMapping list }

type ParseResult =
    { Seeds: int64 list
      CurrentFrom: ItemKind option
      CurrentTo: ItemKind option
      Mappings: AlmanacMapping list }

let parse lines : Input =
    let rec parseR (sofar: ParseResult) (lines: string list) : Input =
        match lines with
        | [] ->
            { Seeds = List.map StartingSeed sofar.Seeds
              Mappings = sofar.Mappings }
        | (h :: t) ->
            match h with
            | _ when System.String.IsNullOrEmpty(h.Trim()) -> parseR sofar t
            | ParseRegex "seeds: (.+)" [ seednrsString ] ->
                let seedNrs =
                    seednrsString.Split(" ")
                    |> Array.map int64
                    |> List.ofArray

                let newSofar =
                    { sofar with Seeds = seedNrs }

                parseR newSofar t
            | ParseRegex "(.+)-to-(.+) map:" [ from; target ] ->
                let fromKind = Map.find from namesOfKinds
                let toKind = Map.find target namesOfKinds

                let newSofar =
                    { sofar with
                        CurrentFrom = Some fromKind
                        CurrentTo = Some toKind }

                parseR newSofar t
            | ParseRegex "(\\d+) (\\d+) (\\d+)" [ destStart; sourceStart; length ] ->
                let newMapping =
                    { RangeMapping = {
                          DestinationRangeStart = (int64 destStart)
                          SourceRangeStart = (int64 sourceStart)
                          RangeLength = (int64 length)
                      }
                      From = sofar.CurrentFrom |> Option.get
                      To = sofar.CurrentTo |> Option.get }

                parseR { sofar with Mappings = (newMapping :: sofar.Mappings) } t
            | _ -> failwith ("todo: " + h)

    parseR
        { Seeds = []
          Mappings = []
          CurrentFrom = None
          CurrentTo = None }
        lines

let processItems (mappings: AlmanacMapping list) (items: Item list) : Item list=    
    let inRange nr (mapping: AlmanacMapping) =
        nr >= mapping.RangeMapping.SourceRangeStart && nr <= mapping.RangeMapping.SourceRangeStart + mapping.RangeMapping.RangeLength - 1L
    
    let processItem item : Item =
        let nr : int64 = item
        let matchingMapping = List.tryFind (inRange item) mappings
        match matchingMapping with
        | None -> 
            (nr)
        | Some m ->
            (nr - m.RangeMapping.SourceRangeStart) + m.RangeMapping.DestinationRangeStart
        
    List.map processItem items

let day5a (almanac: Input) =
    let grouped = List.groupBy (fun (m: AlmanacMapping) -> (m.From, m.To )) almanac.Mappings |> Map.ofList
    let getMappingGroup kind =
        let (f,t) = Map.findKey (fun (from,_) v -> kind = from) grouped
        (Map.find (f,t) grouped), t
        
    let rec processMapping input current =
        match current with
        | Location -> input
        | _ ->
            let currentMapping, nextKind = getMappingGroup current
            let newInput = processItems currentMapping input
            processMapping newInput nextKind

    let startingItems : Item list= List.map (fun (StartingSeed s) -> (s)) almanac.Seeds            
    let mapped = processMapping startingItems Seed
    List.min mapped

type ItemRange = int64 * int64

let processRange (mapping: RangeMapping) ((start, length): ItemRange) : ItemRange list =
    let isBefore =
        start + length - 1L < mapping.RangeLength
    
    if isBefore then
        []        
    else
        []
    

let processRanges (mappings: AlmanacMapping list) (items: ItemRange list) : ItemRange list =
    let processMappings item : ItemRange list =
        List.collect (fun mapping -> processRange mapping item) (List.map (fun bigMapping -> bigMapping.RangeMapping) mappings)        
    List.collect processMappings items

let day5b (almanac: Input) =
    let rec takePairs items =
        match items with
        | [] -> []
        | [ _ ] -> failwith "odd number of items"
        | h1 :: h2 :: t ->
            (h1,h2) :: takePairs t
    
    let startingItems : ItemRange list = takePairs (List.map (fun (StartingSeed n) -> n) almanac.Seeds)
    
    let grouped = List.groupBy (fun (m: AlmanacMapping) -> (m.From, m.To )) almanac.Mappings |> Map.ofList
    let getMappingGroup kind =
        let (f,t) = Map.findKey (fun (from,_) v -> kind = from) grouped
        (Map.find (f,t) grouped), t
        
    let rec processMapping input current =
        match current with
        | Location -> input
        | _ ->
            let currentMapping, nextKind = getMappingGroup current
            let newInput = processRanges currentMapping input
            processMapping newInput nextKind

    let resultingRanges = processMapping startingItems Seed
    List.minBy fst resultingRanges |> fst