module Day14

open Common

type Bitmask = {
    Zeroes: int64;
    Ones: int64;
}

let input = readLines "Day14.txt"

let parseBitmask (bitmask: string) =
    {
        Zeroes = bitmask |> replaceString "X" "1" |> binaryToInt64
        Ones = bitmask |> replaceString "X" "0" |> binaryToInt64
    }

let applyBitmark bitmask input =
    (input &&& bitmask.Zeroes) ||| bitmask.Ones

let updateMemory (memory: Map<int64, int64>) address value =
    memory |> Map.add address value

let instructionFolder (bitmask: Bitmask, memory: Map<int64, int64>) line =
    let lineStart, value = line |> splitByString " = " |> unpack2
    match lineStart with
    | "mask" -> (parseBitmask value, memory)
    | ParseRegex @"mem\[([0-9]+)\]" [ Long address ] -> 
                (bitmask, value |> int64 |> applyBitmark bitmask |> updateMemory memory address)
    | _ -> failwithf "Invalid instruction line: %s" line

let emptyBitmask = {Zeroes = System.Int64.MaxValue; Ones = 0L}

let solveA input =
    let (_, memory) =
        input
        |> Seq.fold instructionFolder (emptyBitmask, Map.empty)
    memory
    |> Map.toList
    |> List.sumBy snd

let allCombinations lst =
    let rec comb accLst elemLst =
        match elemLst with
        | h::t ->
            let next = [h]::List.map (fun el -> h::el) accLst @ accLst
            comb next t
        | _ -> accLst
    comb [] lst

let getBitmaskPermutations (bitmask: string) =
    let baseBitmask = bitmask |> replaceString "X" "0" |> binaryToInt64
    let changedBitmasks =
        bitmask
        |> Seq.rev
        |> Seq.toList
        |> List.indexed
        |> List.filter (fun (_, c) -> c = 'X')
        |> List.map (fun (i, _) -> 1L <<< i)
        |> allCombinations
        |> List.map (List.reduce (|||) >> ((|||) baseBitmask))
    baseBitmask :: changedBitmasks
    

let updateMemoryAtAllPositions (bitmasks: int64 list) (memory: Map<int64, int64>) (address: int64) (value: int64) =
    bitmasks
    |> List.fold (fun mem bitmask -> mem |> Map.add (bitmask ||| address) value) memory

let instructionFolderB (bitmask: int64 list, memory: Map<int64, int64>) line =
    let lineStart, value = line |> splitByString " = " |> unpack2
    match lineStart with
    | "mask" -> (getBitmaskPermutations value, memory)
    | ParseRegex @"mem\[([0-9]+)\]" [ Long address ] -> 
                (bitmask, updateMemoryAtAllPositions bitmask memory address (value |> int64))
    | _ -> failwithf "Invalid instruction line: %s" line

let solveB input =
    let (_, memory) =
        input
        |> Seq.fold instructionFolderB ([], Map.empty)
    memory
    |> Map.toList
    |> List.sumBy snd

let testInput = [
    "mask = 000000000000000000000000000000X1001X"
    "mem[42] = 100"
    "mask = 00000000000000000000000000000000X0XX"
    "mem[26] = 1"
]