module Day09

open Common

let input = readLines "Day09.txt" |> Seq.map int64

let getAllPairs input =
    let indexedInput = Array.indexed input
    Array.allPairs indexedInput indexedInput
    |> Array.filter (fun ((ai, _), (bi, _)) -> ai <> bi)
    |> Array.map (fun (a, b) -> snd a, snd b)

let sumPairExists target pairs =
    pairs
    |> Seq.exists (fun (a, b) -> a + b = target)

let targetIfInvalid input =
    let target = Array.last input
    let sequenceStart = Array.take (Array.length input - 1) input
    let isValid = sequenceStart
                    |> getAllPairs
                    |> sumPairExists target
    match isValid with
    | true -> None
    | false -> Some target

let solveA (input: int64 seq) =
    input
    |> Seq.windowed 26
    |> Seq.map targetIfInvalid
    |> Seq.pick id
    
let findRangeFromStartWhichSumsTo sum (input: int64 list) =
    input
    |> List.indexed
    |> List.tryPick 
        (fun (i, _) -> 
            match input.[..i] with
            | range when List.sum range = sum -> Some range
            | _ -> None 
        )

let solveB input =
    let targetValue = solveA input |> int64
    let inputAsList = input |> Seq.toList
    let range = 
        inputAsList
        |> List.indexed
        |> List.pick (fun (i, _) -> 
                        findRangeFromStartWhichSumsTo targetValue (List.skip i inputAsList))
    (List.min range) + (List.max range)

let solve: int64 seq -> PartToSolve -> Unit = 
    solveDay solveA solveB