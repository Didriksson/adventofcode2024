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

let bounds = (11, 7)

let countForQuadrant (robots: Robot list) ((minx, miny), (maxx, maxy)) =
    robots
    |> List.filter (fun r ->
        let (x, y) = r.position
        x >= minx && x < maxx && y >= miny && y < maxy)
    (*|> List.length*)

let countQuadrants robots (maxx, maxy) =
    let upperleft = ((0, 0), (maxx / 2, maxy / 2))
    let upperright = ((maxx / 2, 0), (maxx, maxy / 2))
    let lowerleft = ((0, maxy / 2), (maxx / 2, maxy))
    let lowerRight = ((maxx / 2, maxy / 2), (maxx, maxy))
    [       lowerleft
      (*
      upperleft ;

      lowerRight ; 
      upperright *) ] |> List.map (countForQuadrant robots)

let robots = File.ReadAllLines "day14/input.txt" |> Seq.toList |> List.map parse

let robotsNewPosition = robots |> List.map (fun r -> performTimes r (11, 7) 100)
countQuadrants robotsNewPosition bounds
