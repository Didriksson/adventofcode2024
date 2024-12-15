open System
open System.IO
open Microsoft.FSharp.Core

let parseLine (line: string) y =
    line
    |> Seq.toList
    |> List.indexed
    |> List.filter (fun (_, it) -> it = 'R')
    |> List.map (fun (idx, it) -> ((idx, y), it))

let parse (input: string list) =
    input
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let rawInput = File.ReadAllLines "day12/input.txt" |> Seq.toList
let input = rawInput |> parse

let evalNode plots visited target =
    plots
    |> List.filter (fun node -> node = target)

let remaining plot visited =
    plot
    |> List.filter (fun node -> (visited |> List.contains node) |> not)

let rec lookForRegion ((x, y), h: char) visited (plot: ((int * int) * char) list) =
    let curriedEval = evalNode plot visited
    let right = curriedEval ((x + 1, y), h)
    let left = curriedEval ((x - 1, y), h)
    let up = curriedEval ((x, y - 1), h)
    let down = curriedEval ((x, y + 1), h)
    let nextToEval = [ right; left; up; down ] |> List.concat
    match nextToEval with
    | [] -> 
        visited
    | vals ->
        vals
        |> List.filter (fun ((x,y), h) -> (visited |> List.contains node) |> not)
        |> List.collect (fun n -> lookForRegion n (((x, y), h) :: visited) plot)
        |> List.distinct

let extractRegion node plot =
    let region = lookForRegion node [node] plot
    printfn $"Starting from {node}, and found {region}"
    printfn $"Remaining {remaining plot region}"
    (remaining plot region, region)


let rec extractTilEmpty potiential regions =
    match potiential with
        | [] -> regions
        | p :: rest -> 
            let (rem, region) = extractRegion p  rest
            extractTilEmpty rem (region :: regions)   

let areaForRegion region =
    region |> List.length

let isRegionPoint garden point =
    garden |> List.exists (fun p -> p = point) |> not

let checkForCropNeighbours garden ((x,y), h) =
    let right = isRegionPoint garden ((x + 1, y), h)
    let left = isRegionPoint garden ((x - 1, y), h)
    let up = isRegionPoint garden ((x, y - 1), h)
    let down = isRegionPoint garden ((x, y + 1), h)
    [ right; left; up; down ] |> List.filter (fun f -> f = true) |> List.length


let perimeter region garden =
    region |> List.sumBy (checkForCropNeighbours garden)

let price r garden =
    let p = perimeter r garden
    let a = areaForRegion r
    p * a

let regions = extractTilEmpty input []
regions |> List.map (fun r ->(r, price r input))