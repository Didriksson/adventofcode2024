open System
open System.IO


let parseRule (l: String) =
    l.Split("|") |> Array.map int |> Array.toList

let parseUpdates (l: String) =
    l.Split(",") |> Array.map int |> Array.toList

let checkRule (rule: int list) (update: int list) =
    let indexFst = update |> List.tryFindIndex (fun it -> it = rule[0])
    let indexSnd = update |> List.tryFindIndex (fun it -> it = rule[1])

    match indexFst, indexSnd with
    | Some val1, Some val2 -> val1 <= val2
    | _ -> true

let correctOrder (rules: int list list) (u: int list) =
    rules |> List.forall (fun r -> checkRule r u)

let rawinput = File.ReadAllLines "day05/input.txt" |> Seq.toList
let rules = rawinput |> List.takeWhile (fun x -> x <> "") |> List.map parseRule

let updates =
    rawinput |> List.skip (1 + (List.length rules)) |> List.map parseUpdates

let correctForRule (update: int list) (rule: int list) =
    let isCorrect = checkRule rule update

    if isCorrect = true then
        update
    else
        let indexFst = update |> List.findIndex (fun it -> it = rule[0])
        let indexSnd = update |> List.findIndex (fun it -> it = rule[1])

        update
        |> List.mapi (fun idx x ->
            if idx = indexFst then update[indexSnd]
            elif idx = indexSnd then update[indexFst]
            else x)

let reorderUpdate (u: int list) (rules: int list list) = rules |> List.fold correctForRule u

let partA updates =
    let matchingUpdates = updates |> List.filter (fun u -> correctOrder rules u)

    matchingUpdates
    |> List.map (fun up -> up[List.length up / 2])
    |> List.reduce (+)

let checkForIncorrectUpdates updates rules =
    updates |> List.filter (fun u -> not (correctOrder rules u))

let rec doUntilCorrect (updates: int list list) (rules: int list list) =
    let reordered = updates |> List.map (fun u -> reorderUpdate u rules)
    let incorrect = checkForIncorrectUpdates reordered rules

    match incorrect with
    | [] -> reordered
    | _ -> doUntilCorrect reordered rules

let incorrect = checkForIncorrectUpdates updates rules
let corrected = doUntilCorrect incorrect rules
corrected |> List.map (fun up -> up[List.length up / 2]) |> List.reduce (+)
