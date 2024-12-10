open System
open System.IO
open Microsoft.FSharp.Core

type FileBlock = {
    id: int
    length: int
    freespace: int
}

let rec parse id acc (toParse: char list) =
    match toParse with
        | [] -> List.rev acc
        | length :: free :: tail ->
            let blocks = { id = id ; length = length |> Char.ToString |> int ; freespace = free |> Char.ToString |> int } :: acc
            parse (id + 1) blocks tail
        | h :: _ ->
            { id = id ; length = h |> Char.ToString |> int ; freespace = 0 } :: acc |> List.rev            


let takeFromLast (tail: FileBlock list) (num: int) =
    match tail with
    | [] -> ([], [])
    | fileBlocks ->
            let last :: rest = List.rev fileBlocks
            let toReturn = List.rev rest
            let toTake = Math.Clamp (num, 0, Math.Min(num, last.length))
            let taken = List.replicate toTake last.id
            if last.length - (List.length taken) = 0 then
                (taken, toReturn)
            else
                (taken, toReturn @ [{ id = last.id ; freespace = last.freespace ; length = last.length - (List.length taken) }])
    

let rec fillFree handled (rem: FileBlock list) =
    match rem with
    | [] -> handled
    | [h] ->
        let file = List.replicate h.length h.id                
        (handled @ file)
    | h :: rest ->
        let file = List.replicate h.length h.id
        let (taken, restOfTail) = takeFromLast rest h.freespace
        let newToHandle = if h.freespace - (List.length taken) = 0 then
                            restOfTail
                          else
                            [{ id = h.id ; freespace = h.freespace - (List.length taken) ; length = 0}] @ restOfTail
        let newhandles = (handled @ file @ taken)
        fillFree newhandles newToHandle 
                    
let blocks = File.ReadAllText "day09/input.txt" |> Seq.toList |> parse 0 [] |> fillFree []
blocks |> List.fold (fun (pos: int64, acc) elem -> (pos + (1 |> int64), acc + (pos * (elem |> int64)))) (0,(0 |> int64))
