open System
open System.IO

let matches c = "#^" |> Seq.toList |> List.contains c

let parseLine (line: string) y =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, it) -> matches it)
    |> List.map (fun (idx, it) -> ((idx, y), it))

let parse (input: string list) =
    input
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let rawInput = File.ReadAllLines "day06/input.txt" |> Seq.toList
let input = rawInput |> parse

let findGuard input =
    input |> List.find (fun (_, v) -> v = '^')


let walk ((x, y), direction) input =
    let nextCoord =
        match direction with
        | '^' -> (x, y - 1)
        | '>' -> (x + 1, y)
        | '<' -> (x - 1, y)
        | 'v' -> (x, y + 1)
        | c -> failwith "Unexpected direction"

    nextCoord

let turn direction =
    match direction with
    | '^' -> '>'
    | '>' -> 'v'
    | '<' -> '^'
    | 'v' -> '<'
    | c -> failwith "Unexpected direction"

let outofbounds (maxX, maxY) (x, y) = x < 0 || x > maxX || y < 0 || y > maxY

let inLoop (guardposition: (int * int) * char) (visited: ((int * int) * char) list) =
    List.contains guardposition visited

let rec walkAndTurn (guardcoord, direction) input bounds (visited: ((int * int) * char) list) =
    let nextStep = walk (guardcoord, direction) input

    if inLoop (nextStep, direction) visited then
        (true, visited)
    elif outofbounds bounds nextStep then
        (false, visited)
    else
        let free = input |> List.contains (nextStep, '#') |> not

        if free then
            walkAndTurn (nextStep, direction) input bounds ((nextStep, direction) :: visited)
        else
            walkAndTurn (guardcoord, turn direction) input bounds visited

let placeobstructionForRow y rowlength input =
    seq { 0 .. (rowlength - 1) }
    |> Seq.map (fun x -> ((x, y), '#') :: input)
    |> Seq.toList


let startGuard = findGuard input

let path =
    walkAndTurn startGuard input (String.length rawInput[0], List.length rawInput) []
    |> snd
    |> List.map fst

let partA = (List.distinct path |> List.length) - 1

let bounds = (String.length rawInput[0], List.length rawInput)

seq { 0 .. (snd bounds) - 1 }
|> Seq.toList
|> List.map (fun y ->
    printfn "Processing row: %d of %d" y ((snd bounds) - 1)
    placeobstructionForRow y (fst bounds) input
    |> List.map (fun i ->
        walkAndTurn startGuard i (String.length rawInput[0], List.length rawInput) []
        |> fst))
|> List.concat
|> List.filter (fun x -> x)
|> List.length