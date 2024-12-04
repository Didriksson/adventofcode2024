open System
open System.IO
open System.Text.RegularExpressions
    

let matchesXmas c =
    "XMAS" |> Seq.toList |> List.contains c

let parseLine (line: string) y=
    line |> Seq.toList |> List.indexed |> List.filter (fun (_, it) -> matchesXmas it) |> List.map (fun (idx, it) -> ((idx, y), it))

let parse (input: string list )=
    input |> List.indexed |> List.map (fun (y, line) -> parseLine line y) |> List.concat

let getForPoints (toFetch: (int * int) list) (points: Map<(int * int), char>) =
     toFetch
         |> List.map (fun p -> (points |> Map.tryFind p))
         |> List.filter _.IsSome
         |> List.map Option.get
         |> String.Concat
let reverse (s: String) =
    s.ToCharArray() |> Array.rev |> String.Concat
    
let checkForPoint ((x, y), _) points =
    let right = getForPoints [(x, y);(x + 1, y); (x + 2, y);(x + 3, y)] points
    let left = getForPoints [(x, y);(x - 1, y); (x - 2, y);(x - 3, y)] points
    let up = getForPoints [(x, y);(x, y  + 1); (x, y  + 2);(x, y  + 3)] points
    let down = getForPoints [(x, y);(x, y - 1); (x, y - 2);(x, y - 3)] points
    let diagupleft = getForPoints [(x, y);(x - 1, y - 1);(x - 2, y - 2); (x - 3, y - 3)] points
    let diagupright = getForPoints [(x, y); (x + 1, y - 1);(x + 2, y - 2); (x + 3, y - 3)] points
    let diagdownleft = getForPoints [(x, y);(x - 1, y + 1); (x - 2, y + 2);(x - 3, y + 3)] points
    let diagdownright = getForPoints [(x, y);(x + 1, y + 1); (x + 2, y + 2);(x + 3, y + 3)] points
    [right ; left ; up ; down ; diagupleft ; diagupright ; diagdownleft ; diagdownright ]
        |> List.map (fun s -> s = "XMAS")
    
let doA rawinput =
    let input = rawinput |> Seq.toList |> parse
    let startingPoints = input |> List.filter (fun (_, it) -> it = 'X')
    let pointsMap = Map.ofList input
    startingPoints |> List.map (fun s -> checkForPoint s pointsMap) |> List.concat |> List.filter (fun b -> b) |> List.length

let checkForPointPartB((x, y), _) points =
    let fstcross = getForPoints [(x - 1, y - 1); (x, y); (x + 1, y + 1)] points
    let sndcross = getForPoints [(x + 1, y - 1); (x, y); (x - 1, y + 1)] points
    (fstcross = "MAS" || fstcross = "SAM" ) && (sndcross = "MAS" || sndcross = "SAM" )
    
let doB rawinput =
    let input = rawinput |> Seq.toList |> parse
    let startingPoints = input |> List.filter (fun (_, it) -> it = 'A')
    let pointsMap = Map.ofList input
    startingPoints |> List.map (fun s -> checkForPointPartB s pointsMap) |> List.filter (fun b -> b) |> List.length
    
let rawinput = File.ReadAllLines "day04/input.txt"
doA rawinput    
doB rawinput



