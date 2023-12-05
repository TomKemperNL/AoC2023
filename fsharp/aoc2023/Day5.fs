module aoc2023.Day5

open Microsoft.FSharp.Reflection
open Shared

type StartingSeed = StartingSeed of int

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

type Item = (int * ItemKind)


type AlmanacMapping =
    { DestinationRangeStart: int
      SourceRangeStart: int
      RangeLength: int
      From: ItemKind
      To: ItemKind }


type Input =
    { Seeds: StartingSeed list
      Mapping: AlmanacMapping list }

type ParseResult =
    { Seeds: int list
      CurrentFrom: ItemKind option
      CurrentTo: ItemKind option
      Mappings: AlmanacMapping list }

let parse lines : Input =
    let rec parseR (sofar: ParseResult) (lines: string list) : Input =
        match lines with
        | [] ->
            { Seeds = List.map StartingSeed sofar.Seeds
              Mapping = sofar.Mappings }
        | (h :: t) ->
            match h with
            | _ when System.String.IsNullOrEmpty(h.Trim()) -> parseR sofar t
            | ParseRegex "seeds: (.+)" [ seednrsString ] ->
                let seedNrs =
                    seednrsString.Split(" ")
                    |> Array.map int
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
                    { DestinationRangeStart = (int destStart)
                      SourceRangeStart = (int sourceStart)
                      RangeLength = (int length)
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


let day5a almanac =
    42
