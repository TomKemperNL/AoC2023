// For more information see https://aka.ms/fsharp-console-apps

open System.Text.RegularExpressions
open aoc2023

printfn "Hello from F#"



open Day7
Map.values cardTranslation |> Seq.sortDescending |> printfn "%A"

