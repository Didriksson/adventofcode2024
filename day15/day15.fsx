open System
open System.IO
open System.Text.RegularExpressions


let parseLine (line: string) y =
    line |> Seq.toList |> List.indexed |> List.map (fun (idx, it) -> ((idx, y), it))

let parseLineB (line: string) y =
    line |> Seq.collect 
            (fun c -> 
                    let newChars = match c with
                                            | '#' -> "##"
                                            | 'O' -> "[]"
                                            | '.' -> ".."
                                            | '@' -> "@."
                                            | _ -> failwith ("Okänd c: "  + string c)
                    newChars
            ) 
         |> Seq.toList |> List.indexed |> List.map (fun (idx, it) -> ((idx, y), it))


let parseGrid (input: string list) =
    input
    |> List.takeWhile (fun l -> l <> "")
    |> List.indexed
    |> List.map (fun (y, line) -> parseLine line y)
    |> List.concat

let parseGridB (input: string list) =
    input
    |> List.takeWhile (fun l -> l <> "")
    |> List.indexed
    |> List.map (fun (y, line) -> parseLineB line y)
    |> List.concat

let canPush (grid: ((int * int) * char) list) (x, y) dx dy maxX maxY =
    let rec checkPush (cx, cy) =
        let nextX, nextY = cx + dx, cy + dy

        if nextX < 0 || nextX >= maxX || nextY < 0 || nextY >= maxY then
            false // Om vi är utanför gränserna, kan vi inte knuffa
        else
            match grid |> List.tryFind (fun ((xx, yy), _) -> (xx, yy) = (nextX, nextY)) with
            | Some(_, '.') -> true // Om nästa plats är tom, kan vi knuffa
            | Some(_, 'O') -> checkPush (nextX, nextY) // Rekursivt, kolla nästa plats
            | Some(_, '[') -> checkPush (nextX, nextY) // Rekursivt, kolla nästa plats
            | Some(_, ']') -> checkPush (nextX, nextY) // Rekursivt, kolla nästa plats
            | _ -> false // Om det är en vägg eller annan hindrande enhet, kan vi inte knuffa

    checkPush (x, y) // Starta från den aktuella positionen

let rec processMove (grid: ((int * int) * char) list) (x, y) (dx, dy) (maxX, maxY) (entity: char) =
    let nextX, nextY = x + dx, y + dy

    if nextX < 0 || nextX >= maxX || nextY < 0 || nextY >= maxY then
        grid // Utanför gränserna, returnera oförändrat
    else
        match grid |> List.tryFind (fun ((cx, cy), _) -> (cx, cy) = (nextX, nextY)) with
        | Some(_, '#') -> grid // Nästa position är en vägg, stopp
        | Some(_, '.') ->
            // Flytta entiteten (antingen robot eller låda)
            grid
            |> List.map (fun ((cx, cy), v) ->
                if (cx, cy) = (x, y) then ((cx, cy), '.') // Nuvarande plats blir tom
                elif (cx, cy) = (nextX, nextY) then ((cx, cy), entity) // Nästa plats fylls
                else ((cx, cy), v))
        | Some(_, 'O') when entity = '@' || entity = 'O' || entity = '[' || entity = ']' ->
            // Om nästa plats är en låda, kolla om vi kan knuffa den
            let kanKnuffa = canPush grid (nextX, nextY) dx dy maxX maxY

            if kanKnuffa then
                // Om vi kan knuffa lådan, uppdatera både robotens och lådans position
                let updatedGrid = processMove grid (nextX, nextY) (dx, dy) (maxX, maxY) 'O'

                updatedGrid
                |> List.map (fun ((cx, cy), v) ->
                    if (cx, cy) = (x, y) then ((cx, cy), '.') // Nuvarande plats för roboten blir tom
                    elif (cx, cy) = (nextX, nextY) then ((cx, cy), entity) // Ny position för roboten
                    else ((cx, cy), v))
            else
                grid // Om vi inte kan knuffa, gör inget
        | _ -> grid // Ingen åtgärd annars



let move (grid: ((int * int) * char) list) (robot: (int * int)) m (bounds: int * int) =
    let delta =
        match m with
        | '<' -> (-1, 0)
        | '^' -> (0, -1)
        | '>' -> (1, 0)
        | 'v' -> (0, 1)
        | _ -> failwith "Invalid move"

    processMove grid robot delta bounds '@'


// Instruktioner för robotens rörelse
let instructions =
    File.ReadAllLines "day15/input.txt"
    |> Seq.skipWhile (fun l -> l <> "")
    |> Seq.skip 1
    |> Seq.concat
    |> Seq.toList

let GPS (x, y) = y * 100 + x


let perform grid = 

    let maxX = grid |> List.map fst |> List.map fst |> List.max
    let maxY = grid |> List.map fst |> List.map snd |> List.max

    instructions
    |> List.fold
        (fun accGrid instr ->
            let robotPos = accGrid |> List.find (fun (_, v) -> v = '@') |> fst
            move accGrid robotPos instr (maxX, maxY))
        grid
    |> List.filter (fun (_, v) -> v = 'O')
    |> List.map fst
    |> List.sumBy GPS



let a =
    let input = File.ReadAllLines "day15/input.txt" 
    let grid = input |> Seq.toList |> parseGrid
    perform grid

let input = File.ReadAllLines "day15/input.txt" 
let grid = input |> Seq.toList |> parseGridB
perform grid