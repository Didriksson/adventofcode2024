open System
open System.IO
open System.Text.RegularExpressions

type Robot =
    { position: int * int
      velocity: int * int }

let parseNumbers b =
    let pattern = @"-?\d+"
    let regex = Regex(pattern)
    let matches = regex.Matches(b)
    let [ a; b ] = matches |> Seq.map _.Value |> Seq.map int |> Seq.toList
    (a, b)

let parse (line: String) =
    let numbers = line.Split(" ") |> Array.map parseNumbers

    { position = numbers[0]
      velocity = numbers[1] }
    : Robot

let performSecond (robot: Robot) (maxx, maxy) =
    let (x, y) = robot.position
    let (velx, vely) = robot.velocity
    let loopedX = ((x + velx) % maxx + maxx) % maxx
    let loopedY = ((y + vely) % maxy + maxy) % maxy

    { position = (loopedX, loopedY)
      velocity = robot.velocity }
    : Robot

let rec performTimes (robot: Robot) bounds times =
    if times = 0 then
        robot
    else
        performTimes (performSecond robot bounds) bounds (times - 1)

let countForQuadrant (robots: Robot list) ((minx, miny), (maxx, maxy)) =
    robots
    |> List.filter (fun r ->
        let (x, y) = r.position
        x >= minx && x < maxx && y >= miny && y < maxy)
    |> List.length

let countQuadrants robots (maxx, maxy) =
    let upperleft = ((0, 0), (maxx / 2, maxy / 2))
    let upperright = ((maxx / 2 + 1, 0), (maxx, maxy / 2))
    let lowerleft = ((0, maxy / 2 + 1), (maxx / 2, maxy))
    let lowerRight = ((maxx / 2 + 1, maxy / 2 + 1), (maxx, maxy))
    [ upperright ; lowerleft  ; upperleft ; lowerRight ]
        |> List.map (countForQuadrant robots)

let robots = File.ReadAllLines "day14/input.txt" |> Seq.toList |> List.map parse

let bounds = (101, 103)


let stringRow (robots: Robot list) row width =
    seq {
        for x in 0 .. width - 1 ->
            let robot = robots |> List.tryFind (fun r -> r.position = (x, row))
            match robot with
            | None -> "."
            | Some _ -> "#"
    } |> Seq.concat |> Seq.map string |> String.concat ""

let print robots (maxx, maxy) = 
    seq {
        for row in 0 .. maxy - 1 do
            stringRow robots row maxx
    }
    |> Seq.toList
          


let robotsNewPosition = robots |> List.map (fun r -> performTimes r bounds 100)
countQuadrants robotsNewPosition bounds |> List.reduce (*)


let rec runTilPrint robots bounds times =
    let res = robots |> List.map (fun r -> performTimes r bounds 1)
    let prints = print res bounds            
    if res |> List.map (_.position) |> List.distinct |> List.length = 500 then
    //if prints |> List.exists (_.Contains("###############################")) = true then
        prints |> List.map Console.WriteLine |> ignore
        Console.WriteLine ("Times: " + (string times))
        exit 1
        runTilPrint res bounds (times + 1)        
    else
        runTilPrint res bounds (times + 1)

(runTilPrint robots bounds 0) |> ignore 

