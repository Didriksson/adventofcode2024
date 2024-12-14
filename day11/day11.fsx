open System
open System.IO
open Microsoft.FSharp.Core

let rawInput = (File.ReadAllText "day11/input.txt").Split(" ") |> Array.map int64 |> Array.toSeq

let blink (stones: int64 seq) =
    stones |> Seq.collect (fun s -> 
        let l = match s with
                            | 0L -> [1 |> int64]
                            | s when (string s |> String.length) % 2 = 0 -> 
                                let strstone = string s 
                                let l = strstone |> String.length
                                [strstone[0..(l/2 - 1)];strstone[l/2..l]] |> List.map int64
                            | _ -> [s * 2024L]
        
        l)
let rec blinkTimes (stones: int64 seq) (times: int) = 
    if times = 0 then
        stones
    else
        let s = blink stones
        let t = times - 1
        blinkTimes s t

blinkTimes  rawInput 75 |> Seq.toList |> List.length