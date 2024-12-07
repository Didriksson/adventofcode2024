open System
open System.IO

let parseLine (line: String) =
    let [| sum; numbers |] = line.Split(": ")
    (sum |> int64, numbers.Split(" ") |> Array.map int64 |> Array.toList)

let allaelement =
    File.ReadAllLines "day07/input.txt" |> Seq.toList |> List.map parseLine

let rec evalAll operatorer tal resten =
    match resten with
    | [] -> [ tal ]
    | h :: tail ->
        operatorer
        |> List.map (fun op -> evalAll operatorer (op tal h) tail)
        |> List.concat

let perform operators =
    allaelement
        |> List.filter
               (fun (summan, h :: tail) -> (evalAll operators h tail) |> List.contains summan)
        |> List.sumBy fst

let concat  a b = (string a) + (string b) |> int64 

let partA = perform [ (+); (*) ]
let partB = perform [ (+); (*) ; concat ]