open System
open System.IO

let matches c = "." |> Seq.toList |> List.contains c

let parseLine (line: string) y =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, it) -> not (matches it))
    |> List.map (fun (idx, it) -> ((idx, y), it))

let parse (input: string list) =
    input
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let input = File.ReadAllLines "day08/input.txt" |> Seq.toList

let radios = input |> parse
let bounds = (String.length input[0], List.length input)

let distanceBetween (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

let findAntinodes (x1, y1) (x2, y2) =
    let dx, dy = distanceBetween (x1, y1) (x2, y2)
    [ (x1 - dx, y1 - dy); (x2 + dx, y2 + dy) ]

let handleFrequency f input freqfunction =
    let antennas =
        input |> List.filter (fun n -> (snd n) = f) |> List.map fst

    antennas
    |> List.map (fun a1 ->
        antennas
        |> List.filter (fun a2 -> a2 <> a1)
        |> List.map (freqfunction a1)
        |> List.concat)
    |> List.concat

let outofbounds (maxX, maxY) (x, y) =
    x < 0 || x >= maxX || y < 0 || y >= maxY

let frequencies = radios |> List.map snd |> List.distinct

let doIt mapFunction=
    frequencies
    |> List.map (fun f -> handleFrequency f radios mapFunction)
    |> List.concat
    |> List.distinct
    |> List.filter (fun n -> not (outofbounds bounds n))
    |> List.length

let resonantHarmonics bounds (x1, y1) (x2, y2) =
    let dx, dy = distanceBetween (x1, y1) (x2, y2)
    let rec loop (x, y) mulfunction =
        let newpoint = ((mulfunction x dx), (mulfunction y dy))
        if not (outofbounds bounds newpoint) then
            newpoint :: loop newpoint mulfunction
        else
            [(x, y)]
    [(loop (x1, y1) (-)) ; (loop (x2, y2) (+)) ; (loop (x1, y1) (+)) ; (loop (x2, y2) (-)) ; [(x1, y1)] ; [(x2, y2)]]
        |> List.concat    

let partA = doIt findAntinodes
let partB = doIt (resonantHarmonics bounds)