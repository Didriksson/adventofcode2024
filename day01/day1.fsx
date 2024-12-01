open System
open System.IO


let parse (line: String) =
    let split = line.Split("   ")
    (split[0], split[1])

let solveA listA listB =
    List.zip (List.sort listA) (List.sort listB)
        |> List.map (fun (x, y) -> x - y) |> List.map abs |> List.sum

let similarityScore (listB: int list) (it: int) =
    let times = listB |> List.filter (fun x -> x = it) |> List.length
    times * it

let solveB (listA: int list) (listB: int list) =
    listA |> List.map (similarityScore listB) |> List.sum

let parseLines = File.ReadLines "day01/input.txt" |> Seq.toList |> List.map parse
let a = parseLines |> List.map fst |> List.map int
let b = parseLines |> List.map snd |> List.map int

solveB a b