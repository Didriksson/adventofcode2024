open System
open System.IO
open Microsoft.FSharp.Core
open System.Text.RegularExpressions
 
type ClawMachine = {
    a: int64 * int64
    b: int64 * int64
    prize: int64 * int64
}
 
let parseNumbers b =
    let pattern = @"\d+"
    let regex = Regex(pattern)
    let matches = regex.Matches(b)
    let [a; b] = matches |> Seq.map (fun f -> f.Value) |> Seq.map int |> Seq.toList
    (int64 a, int64 b)
 
let parseClawmachine [a ; b ; price] =
    {
        a = parseNumbers a
        b = parseNumbers b
        prize = parseNumbers price
    } : ClawMachine
 
let clawmachines = File.ReadAllLines "day13/input.txt" 
                                    |> Seq.toList
                                    |> List.filter (fun l -> l <> "")
                                    |> List.chunkBySize 3
                                    |> List.map parseClawmachine
                                    
let rec gcd x y =
    if y = 0 then x
    else gcd y (x % y)
 
(*let isSolvable (clawMachine: ClawMachine) =
    let (x1, y1) = clawMachine.a
    let (x2, y2) = clawMachine.b
    let gcd1 = gcd x1 x2
    let gcd2 = gcd y1 y2
    (clawMachine.prize |> fst) % gcd1 = 0 && (clawMachine.prize |> fst) % gcd2 = 0  *)  
 
let pushButton (x:int64, y:int64) times =
    (x * times, y * times)
 
let pushButtons (clawmachine: ClawMachine) atimes btimes =
    let (x1, y1) = (pushButton clawmachine.a atimes)
    let (x2, y2) = (pushButton clawmachine.b btimes)
    (x1 + x2, y1 + y2)
 
let winnerwinner (clawmachine: ClawMachine) coord =
    clawmachine.prize = coord
    
let (height, width) = (101, 101)

// let solvable = clawmachines |> List.filter isSolvable

let price (a,b) =
    a * 3 + b

let solveForClawMachine c =
    seq {
    for button1 in 1 .. width - 1 do
        for button2 in 1 .. height - 1 -> (button1, button2)
    }
      |> Seq.toList
      |> List.map (fun (b1, b2) -> ((b1,b2), pushButtons c b1 b2))
      |> List.filter (fun (coord, res) -> winnerwinner c res)
      |> List.map fst
      |> List.map price
      
      
let convertPartB (c: ClawMachine) =
    let (x,y) = c.prize 
    { a  = c.a ; b = c.b ; prize =  (x + 10000000000000L, y + 10000000000000L) }
    
let a = clawmachines |> List.map solveForClawMachine |> List.filter (fun f -> not (f.IsEmpty)) |> List.sumBy List.min
        

                    