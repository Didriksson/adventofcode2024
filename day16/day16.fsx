open System
open System.IO
open Microsoft.FSharp.Core

let parseLine (line: string) y =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, it) -> it <> '#')
    |> List.map (fun (idx, it) -> ((idx, y), it))

let parse (input: string list) =
    input
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let rawInput = File.ReadAllLines "day16/input.txt" |> Seq.toList
let input = rawInput |> parse

let evalNode trails visited (x, y) =
    trails
    |> List.filter (fun ((ix, iy), _) -> (ix, iy) = (x, y)) // Grannar med samma koordinater
    |> List.filter (fun node -> not (List.contains node visited)) // Ej besökta

type Directions = NORTH | WEST | EAST | SOUTH 


let rec lookForPath (queue: (((int * int) * char) * int * Directions) list) visited target  input =
    match queue with
        | [] -> Int32.MaxValue
        | (((x,y), v), score,  direction) :: rest ->
            if v = target then
                score
            else
                let curriedEval = evalNode input visited
                let right = curriedEval (x + 1, y)
                let left = curriedEval (x - 1, y)
                let up = curriedEval (x, y - 1)
                let down = curriedEval (x, y + 1)
                let newVisited = ((x,y), v) :: visited
                let newQueue = 
                    [ right; left; up; down ] |> List.concat |> List.map (fun (n) -> 
                        let ((x2, y2), _) = n
                        let newDirection = match (x2 - x, y2 - y) with
                                                    | (0, 1) -> SOUTH
                                                    | (0, -1) -> NORTH
                                                    | (1, 0) -> EAST
                                                    | (-1, 0) -> WEST
                                                    | _ -> failwith "Okänd riktning"
                        
                        let turncost = if newDirection = direction then 1 else 1000 + 1
                        (n, score + turncost, newDirection))
                    |> (@) rest
                    |> List.sortBy (fun (_, cost, _) -> cost)
                lookForPath newQueue newVisited target input
                    
let startpoint = input |> List.filter (fun (_, v) -> v = 'S') |> List.head

let partA =  lookForPath [(startpoint, 0, EAST)]  [] 'E' input