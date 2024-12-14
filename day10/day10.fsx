open System
open System.IO
open Microsoft.FSharp.Core

let parseLine (line: string) y =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, it) -> it <> '.')
    |> List.map (fun (idx, it) -> ((idx, y), it |> Char.ToString |> int))

let parse (input: string list) =
    input
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let rawInput = File.ReadAllLines "day10/input.txt" |> Seq.toList
let input = rawInput |> parse

let evalNode trails visited target =
    trails
    |> List.filter (fun node -> node = target)
    |> List.filter (fun node -> (visited |> List.contains node) |> not)

let rec lookForPath ((x, y), h) visited target trails =
    if h = target then
        [ Some(visited @ [ (x, y), h ]) ]
    else
        let curriedEval = evalNode trails visited
        let right = curriedEval ((x + 1, y), h + 1)
        let left = curriedEval ((x - 1, y), h + 1)
        let up = curriedEval ((x, y - 1), h + 1)
        let down = curriedEval ((x, y + 1), h + 1)
        let nextToEval = [ right; left; up; down ] |> List.concat

        match nextToEval with
        | [] -> [ Option.None ]
        | vals ->
            vals
            |> List.collect (fun n -> lookForPath n (visited @ [ ((x, y), h) ]) target trails)
            |> List.distinct

let startpoints = input |> List.filter (fun (_, v) -> v = 0)

let partA =  startpoints
                    |> List.map (fun s -> lookForPath s [] 9 input)
                    |> List.map (fun o -> o |> List.map (Option.map List.last))
                    |> List.map (List.filter Option.isSome)
                    |> List.map (List.map Option.get)
                    |> List.map List.distinct 
                    |> List.sumBy List.length

let partB = startpoints
                    |> List.map (fun s -> lookForPath s [] 9 input)
                    |> List.map (List.filter Option.isSome)
                    |> List.map List.length
                    |> List.sum