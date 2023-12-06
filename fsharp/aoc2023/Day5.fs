module aoc2023.Day5

open System.Reflection
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

type Item = (int64 * ItemKind)


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

let processItems nextkind (mappings: AlmanacMapping list) (items: Item list) : Item list=    
    let inRange nr (mapping: AlmanacMapping) =
        nr >= mapping.RangeMapping.SourceRangeStart && nr <= mapping.RangeMapping.SourceRangeStart + mapping.RangeMapping.RangeLength - 1L
    
    let processItem item : Item =
        let nr, _ = item
        let matchingMapping = List.tryFind (inRange nr) mappings
        match matchingMapping with
        | None -> 
            (nr, nextkind)
        | Some m ->
            (nr - m.RangeMapping.SourceRangeStart) + m.RangeMapping.DestinationRangeStart, nextkind
        
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
            let newInput = processItems nextKind currentMapping input
            processMapping newInput nextKind

    let startingItems : Item list= List.map (fun (StartingSeed s) -> (s, Seed)) almanac.Seeds            
    let mapped = processMapping startingItems Seed
    List.minBy fst mapped |> fst

type ItemRange = int64 * int64 * ItemKind

let rec processRange (from, target) (mapping: RangeMapping) ((start, length, kind): ItemRange) : ItemRange list =
    if kind <> from then
        [(start,length, kind)] //Inapplicable mapping
    else if start + length - 1L < mapping.SourceRangeStart then
        [(start,length, kind)] //Completely in front
    else if start > (mapping.SourceRangeStart + mapping.RangeLength - 1L) then
        [(start,length, kind)] //Completely after
    else if start < mapping.SourceRangeStart then //Overlap Front
        let firstRangeStart = start
        let firstRangeLength = (mapping.SourceRangeStart - start)
        let secondRangeLength = length - firstRangeLength
        let secondRangeStart = mapping.SourceRangeStart
        
        (firstRangeStart, firstRangeLength, kind) :: processRange (from, target) mapping (secondRangeStart, secondRangeLength, kind)
    else if (start + length) <= (mapping.SourceRangeStart + mapping.RangeLength) then //Contained
        let offset = start - mapping.SourceRangeStart
        [(mapping.DestinationRangeStart + offset, length, target)]
    else //Overlap after
        let offset = start - mapping.SourceRangeStart
        let firstRangeStart = mapping.DestinationRangeStart + offset
        let firstRangeLength = mapping.RangeLength - offset
        let secondRangeStart = mapping.SourceRangeStart + mapping.RangeLength
        let secondRangeLength = length - firstRangeLength
        
        (firstRangeStart, firstRangeLength, target) :: processRange (from, target) mapping (secondRangeStart, secondRangeLength, from)
    

let processRanges (from,target) (mappings: AlmanacMapping list) (items: ItemRange list) : ItemRange list =  
    
    
    
    let processMappings items mapping : ItemRange list =
        List.collect (processRange (from,target) mapping) items       
                
    List.fold processMappings items (List.map (fun m -> m.RangeMapping) mappings)
    |> List.map (fun (n, l, _) -> (n,l, target)) 
    

let day5b (almanac: Input) =
    let rec takePairs items =
        match items with
        | [] -> []
        | [ _ ] -> failwith "odd number of items"
        | h1 :: h2 :: t ->
            (h1,h2) :: takePairs t
    
    let startingItems : ItemRange list =
        takePairs (List.map (fun (StartingSeed n) -> n) almanac.Seeds)
        |> List.map (fun (nr, length) -> nr, length, Seed)
    
    let grouped = List.groupBy (fun (m: AlmanacMapping) -> (m.From, m.To )) almanac.Mappings |> Map.ofList
    let getMappingGroup kind =
        let (f,t) = Map.findKey (fun (from,_) v -> kind = from) grouped
        (Map.find (f,t) grouped), t
        
    let rec processMapping input current =
        match current with
        | Location -> input
        | _ ->
            let currentMapping, nextKind = getMappingGroup current
            let newInput = processRanges (current, nextKind) currentMapping input
            processMapping newInput nextKind

    let resultingRanges = processMapping startingItems Seed
    let result, _,_ = List.minBy (fun (nr, l, k) -> nr) resultingRanges
    result 