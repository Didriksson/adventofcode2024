open System
open System.IO
open Microsoft.FSharp.Core

let parseLine (line: string) y =
    line |> Seq.toList |> List.indexed |> List.map (fun (idx, it) -> ((idx, y), it))

let parse (input: string list) =
    input
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let rawInput = File.ReadAllLines "day12/input.txt" |> Seq.toList
let input = rawInput |> parse

let remaining plot visited =
    plot |> List.filter (fun node -> (visited |> List.contains node) |> not)

let rec lookForRegion
    (visited: Set<(int * int) * char>)
    (toVisit: ((int * int) * char) list)
    (plot: ((int * int) * char) list)
    =
    match toVisit with
    | [] -> visited |> Set.toList
    | node :: rest ->
        let (nx, ny), nh = node

        let neighbors =
            [ ((nx + 1, ny), nh)
              ((nx - 1, ny), nh)
              ((nx, ny + 1), nh)
              ((nx, ny - 1), nh) ]
            |> List.filter (fun n -> List.contains n plot)
            |> List.filter (fun n -> not (visited.Contains n))
            |> List.filter (fun n -> not (List.contains n toVisit))

        // Update queue and visited set
        let newToVisit = rest @ neighbors
        let newVisited = visited.Add(node)

        // Tail-recursive call
        lookForRegion newVisited newToVisit plot

let extractRegion node plot =
    let region = lookForRegion (Set.ofList [ node ]) [ node ] plot |> List.distinct
    let remaining = plot |> List.filter (fun n -> not (List.contains n region))
    (remaining, region)

let rec extractTilEmpty potiential regions =
    match potiential with
    | [] -> regions
    | p :: rest ->
        let (rem, region) = extractRegion p rest
        extractTilEmpty rem (region :: regions)

let areaForRegion region = region |> List.length

let isNotPointInRegion garden point =
    garden |> List.exists (fun p -> p = point) |> not

let checkForCropNeighbours garden ((x, y), h) =
    let right = isNotPointInRegion garden ((x + 1, y), h)
    let left = isNotPointInRegion garden ((x - 1, y), h)
    let up = isNotPointInRegion garden ((x, y - 1), h)
    let down = isNotPointInRegion garden ((x, y + 1), h)
    [ right; left; up; down ]

let perimeter region garden =
    region
    |> List.collect (checkForCropNeighbours garden)
    |> List.filter (fun f -> f = true)
    |> List.length

let price r garden =
    let p = perimeter r garden
    let a = areaForRegion r
    p * a

let checkForCorners garden ((x, y), h) =
    let right = isNotPointInRegion garden ((x + 1, y), h)
    let left = isNotPointInRegion garden ((x - 1, y), h)
    let up = isNotPointInRegion garden ((x, y - 1), h)
    let down = isNotPointInRegion garden ((x, y + 1), h)
    let diagonalUpRight = isNotPointInRegion garden ((x + 1, y - 1), h)
    let diagonalDownRight = isNotPointInRegion garden ((x + 1, y + 1), h)
    let diagonalDownLeft = isNotPointInRegion garden ((x - 1, y + 1), h)
    let diagonalUpLeft = isNotPointInRegion garden ((x - 1, y - 1), h)

    let topLeft = up && left
    let bottomLeft = down && left
    let topRight = up && right
    let bottomRight = down && right
    let innercorner1 = (not up && not right && diagonalUpRight)
    let innercorner2 = (not down && not left && diagonalDownLeft)
    let innercorner3 = (not down && not right && diagonalDownRight)
    let innercorner4 = (not up && not left && diagonalUpLeft)


    [ topLeft
      bottomLeft
      topRight
      bottomRight
      innercorner1
      innercorner2
      innercorner3
      innercorner4 ]
        |> List.filter (fun f -> f = true)
        |> List.length

let countSides region garden =
    (region, region |> List.map (checkForCorners garden)
    |> List.sum
    )

let regions = extractTilEmpty input []
let partA = regions |> List.sumBy (fun r -> price r input)

regions
|> List.map (fun r -> countSides r input)
|> List.sumBy (fun (r, s) -> s * areaForRegion r)
