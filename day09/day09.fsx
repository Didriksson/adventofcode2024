open System
open System.IO
open Microsoft.FSharp.Core

type FileBlock =
    { id: int; length: int; freespace: int }

let rec parse id acc (toParse: char list) =
    match toParse with
    | [] -> List.rev acc
    | length :: free :: tail ->
        let blocks =
            { id = id
              length = length |> Char.ToString |> int
              freespace = free |> Char.ToString |> int }
            :: acc

        parse (id + 1) blocks tail
    | h :: _ ->
        { id = id
          length = h |> Char.ToString |> int
          freespace = 0 }
        :: acc
        |> List.rev


let takeFromLast (tail: FileBlock list) (num: int) =
    match tail with
    | [] -> ([], [])
    | fileBlocks ->
        let last :: rest = List.rev fileBlocks
        let toReturn = List.rev rest
        let toTake = Math.Clamp(num, 0, Math.Min(num, last.length))
        let taken = List.replicate toTake last.id

        if last.length - (List.length taken) = 0 then
            (taken, toReturn)
        else
            (taken,
             toReturn
             @ [ { id = last.id
                   freespace = last.freespace
                   length = last.length - (List.length taken) } ])


let rec fillFree handled (rem: FileBlock list) =
    match rem with
    | [] -> handled
    | [ h ] ->
        let file = List.replicate h.length h.id
        (handled @ file)
    | h :: rest ->
        let file = List.replicate h.length h.id
        let taken, restOfTail = takeFromLast rest h.freespace

        let newToHandle =
            if h.freespace - (List.length taken) = 0 then
                restOfTail
            else
                [ { id = h.id
                    freespace = h.freespace - (List.length taken)
                    length = 0 } ]
                @ restOfTail

        let newhandles = (handled @ file @ taken)
        fillFree newhandles newToHandle

let rec takeFreeSpace (block: FileBlock) (blocks: FileBlock list) handled =
    if block.length = 0 then
        blocks
    else
        match blocks with
        | [] -> block :: handled |> List.rev
        | [x] ->
            if x.freespace - block.length >= 0 then
                let updatedBlock =
                    { id = block.id
                      freespace =  block.freespace + block.length
                      length = block.length }
                    
                let nodeThatHaveSpaceFilled =
                    {   id = x.id
                        freespace = 0
                        length = x.length }
                
                (updatedBlock :: (nodeThatHaveSpaceFilled :: handled) |> List.rev)
            else
                takeFreeSpace block [] (x :: handled)
        | x :: xs ->
            if x.freespace - block.length >= 0 then
                let spaceLeft = x.freespace - block.length

                let updatedBlock =
                    { id = block.id
                      freespace = spaceLeft
                      length = block.length }
                    
                let nodeThatHaveSpaceFilled =
                    {   id = x.id
                        freespace = 0
                        length = x.length }
                    
                match xs with
                | [] ->
                    (updatedBlock :: (nodeThatHaveSpaceFilled :: handled) |> List.rev)
                | _ ->
                    let nodeNextToNodeLeftBehind = List.last xs
                    let updatedLastNode =
                        { id = nodeNextToNodeLeftBehind.id
                          freespace = nodeNextToNodeLeftBehind.freespace + block.freespace + block.length
                          length = nodeNextToNodeLeftBehind.length }

                    (updatedBlock :: (nodeThatHaveSpaceFilled :: handled) |> List.rev)
                    @ (List.take ((List.length xs) - 1) xs)
                    @ [ updatedLastNode ]
            else
                takeFreeSpace block xs (x :: handled)

let moveFile id (blocks: FileBlock list) =
    let front = blocks |> List.takeWhile (fun b -> b.id <> id)
    let rest = blocks |> List.skipWhile (fun b -> b.id <> id)

    match rest with
    | [] -> blocks
    | block :: back ->
        let updated = (takeFreeSpace block front [])
        updated @ back

let blocks = File.ReadAllText "day09/input.txt" |> Seq.toList |> parse 0 []

let partA =
    blocks
    |> fillFree []
    |> List.fold (fun (pos: int64, acc) elem -> (pos + (1 |> int64), acc + (pos * (elem |> int64)))) (0, (0 |> int64))

let prettyPrint (b: FileBlock) =
    (List.replicate b.length (string b.id) |> String.Concat)
    + (List.replicate b.freespace "." |> String.Concat)

let foldSum (pos: int64, acc) elem =
    let sum = {pos .. (pos+(elem.length |> int64) - (1 |> int64))} |> Seq.map (fun idx -> (idx * (elem.id |> int64))) |> Seq.reduce (+)
    ((pos + (elem.length |> int64) + (elem.freespace |> int64)), acc + sum)

let partB =
    blocks
    |> List.rev
    |> List.fold (fun upblocks it ->
//        let ughf = upblocks |> List.map prettyPrint |> String.Concat
//        printfn $"Före:  {ughf}"
        let fisksoppa = moveFile it.id upblocks
//        let ugh = fisksoppa |> List.map prettyPrint |> String.Concat
//        printfn $"Efter: {ugh}"
        fisksoppa
        ) blocks
    |> Seq.fold foldSum (0, (0 |> int64))

