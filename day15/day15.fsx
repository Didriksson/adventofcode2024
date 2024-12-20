open System
open System.IO
open System.Text.RegularExpressions


let parseLine (line: string) y =
    line |> Seq.toList |> List.indexed |> List.map (fun (idx, it) -> ((idx, y), it))

let parseGrid (input: string list) =
    input
    |> List.takeWhile (fun l -> l <> "")
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat


let rec processMove (grid: ((int * int) * char) list) (x, y) (dx, dy) (maxX, maxY) (entity: char) =
    let nextX, nextY = x + dx, y + dy

    if nextX < 0 || nextX >= maxX || nextY < 0 || nextY >= maxY then
        grid // Utanför gränserna, returnera oförändrat
    else
        match grid |> List.tryFind (fun ((cx, cy), _) -> (cx, cy) = (nextX, nextY)) with
        | Some (_, '#') -> grid // Nästa position är en vägg, stopp
        | Some (_, '.') ->
            // Flytta entiteten (antingen robot eller låda)
            grid
            |> List.map (fun ((cx, cy), v) ->
                if (cx, cy) = (x, y) then ((cx, cy), '.') // Nuvarande plats blir tom
                elif (cx, cy) = (nextX, nextY) then ((cx, cy), entity) // Nästa plats fylls
                else ((cx, cy), v))
        | Some (_, '0') when entity = '@' || entity = '0' ->
            // Knuffa lådan och fortsätt med kedjerörelsen
            let updatedGrid = processMove grid (nextX, nextY) (dx, dy) (maxX, maxY) '0'
            updatedGrid
            |> List.map (fun ((cx, cy), v) ->
                if (cx, cy) = (x, y) then ((cx, cy), '.') // Nuvarande plats blir tom
                elif (cx, cy) = (nextX, nextY) then ((cx, cy), entity) // Ny position
                else ((cx, cy), v))
        | _ -> grid // Ingen åtgärd annars


let move (grid: ((int * int) * char) list) (robot: (int * int)) m (bounds: int * int) =
    let delta = match m with
                | '<' -> (-1, 0)
                | '^' -> (0, -1)
                | '>' -> (1, 0)
                | 'v' -> (0, 1)
                | _ -> failwith "Invalid move"
    processMove grid robot delta (fst bounds, snd bounds) '@'
    
let input = File.ReadAllLines "day15/input.txt" |> Seq.toList |> parseGrid
let robot = input |> Seq.find (fun (_, c) -> c = '@') |> fst
let grid = input
let maxX = grid |> List.map fst |> List.map fst |> List.max
let maxY = grid |> List.map fst |> List.map snd |> List.max

// Instruktioner för robotens rörelse
let instructions = File.ReadAllLines "day15/input.txt" |> Seq.skipWhile (fun l -> l <> "") |> Seq.skip 1 |> Seq.concat |> Seq.toList

// Flytta roboten enligt instruktioner
let updatedGrid = instructions |> List.fold (fun accGrid instr -> 
    let robotPos = accGrid |> List.find (fun (_, v) -> v = '@') |> fst
    move accGrid robotPos instr (maxX, maxY)) grid