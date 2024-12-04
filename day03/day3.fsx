open System
open System.IO
open System.Text.RegularExpressions

let parseAndSum matches =
    let toMul = matches |> List.map (fun x ->
        Regex.Matches(x, "(\d+)") |> Seq.map _.Value |> Seq.toList |> List.map int)
    toMul |> List.map (fun x -> x[0] * x[1]) |> List.sum

let doA input =
    let matches = Regex.Matches(input, "mul\(\d+\,\d+\)") |> Seq.map _.Value |> Seq.toList
    parseAndSum matches
    
let rec filterMatches toMatch matches =
    match toMatch with
    | [] -> List.rev matches
    | "don't()" :: rest -> filterMatches (rest |> List.skipWhile (fun x -> x <> "do()")) matches
    | "do()" :: rest -> filterMatches rest matches
    | h :: rest -> filterMatches rest (h::matches)

let doB input =
    let matches = Regex.Matches(input, "mul\(\d+\,\d+\)|don't\(\)|do\(\)") |> Seq.map _.Value |> Seq.toList
    let filterdmatches = filterMatches matches []
    parseAndSum filterdmatches
    
let input = File.ReadAllText "day03/input.txt"
let partA = doA input
let partB = doB input
