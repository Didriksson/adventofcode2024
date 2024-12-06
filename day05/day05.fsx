open System
open System.IO
    

let parseRule (l: String) =
    l.Split ("|") |> Array.map int |> Array.toList
let parseUpdates (l:String) = 
    l.Split (",") |> Array.map int |> Array.toList

let checkRule (rule: int list) (update: int list) =
    let indexFst = update |> List.tryFindIndex (fun it -> it = rule[0])
    let indexSnd = update |> List.tryFindIndex (fun it -> it = rule[1])

    match indexFst, indexSnd with
    | Some val1, Some val2 -> val1 <= val2
    | _ -> true
            

let correctOrder (rules: int list list) (u: int list) =
    rules |> List.forall (fun r -> checkRule r u)

let rawinput = File.ReadAllLines "day05/input.txt" |> Seq.toList
let rules = 
    rawinput 
    |> List.takeWhile (fun x -> x <> "")
    |> List.map parseRule

let updates= 
    rawinput 
    |> List.skip (1 + (List.length rules))
    |> List.map parseUpdates

let correctForRule (r: int list) (u: int list) =
    let checkRule (rule: int list) (update: int list) =
    let indexFst = update |> List.tryFindIndex (fun it -> it = rule[0])
    let indexSnd = update |> List.tryFindIndex (fun it -> it = rule[1])

    match indexFst, indexSnd with
    | Some val1, Some val2 -> val1 <= val2
    | _ -> true

let reorderUpdate (u: int list) (rules: int list list) =
    let breakingRules = rules |> List.filter (fun r -> (not (checkRule r u)))
    breakingRules |> List.map (fun correctForRule r u)  

let partA updates = 
    let matchingUpdates = updates |> List.filter (fun u -> correctOrder rules u)
    matchingUpdates |> List.map (fun up -> up[List.length up / 2]) |> List.reduce (+)

let incorrectUpdates = updates |> List.filter (fun u -> not (correctOrder rules u))
incorrectUpdates |> List.map (fun iu -> reorderUpdate iu rules)


