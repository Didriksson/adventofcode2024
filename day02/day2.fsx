open System
open System.IO


let parse (line: String) =
    line.Split(" ") |> Array.map int |> Array.toList

let reports = File.ReadLines "day02/input.txt" |> Seq.toList |> List.map parse


let decreasing (a: int,b: int) = a > b
let increasing (a: int,b: int) = a < b
let maxDistance (a: int,b: int) =
    let distance = (a - b) |> abs
    distance > 0 && distance < 4 

let isSafe (report : int list)=
    let pairs = List.pairwise report
    List.forall maxDistance pairs &&
    (List.forall increasing pairs || List.forall decreasing pairs) 

let remove1Safe report =
    let mutations = List.indexed report |> List.map (fun (idx, _) -> List.removeAt idx report)
    List.exists isSafe mutations

let partA = reports |> List.filter isSafe |> List.length
reports |> List.filter remove1Safe |> List.length