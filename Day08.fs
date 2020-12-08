module Day08

open Common

type Instruction = 
    | Acc of int
    | Jmp of int
    | Nop of int

let input = readLines "Day08.txt"

let parseLine line =
    match (line |> splitBy ' ' |> unpack2) with
    | "acc", Integer x -> Acc x
    | "jmp", Integer x -> Jmp x
    | "nop", Integer x -> Nop x
    | invalid -> failwithf "Invalid input line %A" invalid

let solveA input =
    let instructionList =
        input
        |> Seq.map parseLine
        |> Seq.toList
    
    let rec executeInstruction (acc: int) (instructionCount: int) (alreadyVisited: int list) =
        if List.contains instructionCount alreadyVisited then 
            acc
        else
            let nextAlreadyVisited = alreadyVisited @ [instructionCount]
            match List.item instructionCount instructionList with
            | Acc x -> executeInstruction (acc + x) (instructionCount + 1) nextAlreadyVisited
            | Jmp x -> executeInstruction (acc) (instructionCount + x) nextAlreadyVisited
            | Nop _ -> executeInstruction (acc) (instructionCount + 1) nextAlreadyVisited
    
    executeInstruction 0 0 []


let solveB input =
    let instructionList =
        input
        |> Seq.map parseLine
        |> Seq.toList

    let rec executeInstruction (acc: int) (instructionCount: int) (alreadyVisited: int list) (modified: bool) =
        match instructionCount with
        | count when List.contains count alreadyVisited -> None
        | count when count < 0 || count > List.length instructionList -> None
        | count when count = List.length instructionList -> Some acc
        | _ ->
            let nextAlreadyVisited = alreadyVisited @ [instructionCount]

            match (List.item instructionCount instructionList), modified with
            | Acc x, _ -> executeInstruction (acc + x) (instructionCount + 1) nextAlreadyVisited modified
            | Jmp x, true -> executeInstruction (acc) (instructionCount + x) nextAlreadyVisited true
            | Nop _, true -> executeInstruction (acc) (instructionCount + 1) nextAlreadyVisited true
            | Jmp x, false -> List.tryPick id [
                                executeInstruction (acc) (instructionCount + x) nextAlreadyVisited false;
                                executeInstruction (acc) (instructionCount + 1) nextAlreadyVisited true
                                ]
            | Nop x, false -> List.tryPick id [
                                executeInstruction (acc) (instructionCount + x) nextAlreadyVisited true;
                                executeInstruction (acc) (instructionCount + 1) nextAlreadyVisited false
                                ]

    executeInstruction 0 0 [] false

let solve: string seq -> PartToSolve -> Unit = solveDay solveA solveB

let testInput = [
    "nop +0"
    "acc +1"
    "jmp +4"
    "acc +3"
    "jmp -3"
    "acc -99"
    "acc +1"
    "jmp -4"
    "acc +6"
]