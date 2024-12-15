open System
open System.IO
open Microsoft.FSharp.Core
open System.Collections.Generic

let rawInput = (File.ReadAllText "day11/input.txt").Split(" ") |> Array.map int64 |> Array.toSeq

let memoize f =
    let cache = Dictionary<_, _>()

    fun x ->
        match cache.TryGetValue x with
        | true, value -> value
        | false, _ ->
            let value = f x
            cache.Add(x, value)
            value
let blinkStone stone =
    match stone with
        | 0L -> [1L]
        | s when (string s |> String.length) % 2 = 0 -> 
            let strstone = string s 
            let l = strstone |> String.length
            [strstone[0..(l/2 - 1)];strstone[l/2..l]] |> List.map int64
        | _ -> [stone * 2024L]
let blinkfunction = memoize blinkStone
let rec blinkTimes =
    fun (times,stone) ->
        if times = 0 then
            1L
        else
            let ss = blinkfunction stone
            let t = times - 1
            ss |> Seq.sumBy (fun s -> blinkTimes  (t, s))
    |> memoize


let a = rawInput |> Seq.sumBy (fun s -> blinkTimes (25, s))
let b = rawInput |> Seq.sumBy (fun s -> blinkTimes (75, s))