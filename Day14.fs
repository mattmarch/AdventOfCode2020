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

let rec applyBitmaskOptions bitsToChange address =
    let bitToChange = Seq.head bitsToChange
    let nextBits = Seq.tail bitsToChange
    let withBitNegative = address &&& ~~~ (1L <<< bitToChange)
    let withBitPositive = address ||| (1L <<< bitToChange)
    match nextBits with
    | nextBitsToChange when Seq.isEmpty nextBitsToChange -> [withBitNegative; withBitPositive]
    | _ -> List.concat [
        applyBitmaskOptions nextBits withBitNegative;
        applyBitmaskOptions nextBits withBitPositive;
        ]

let applyAllBitmaskOptionsToAddress (bitmask: string) (address: int64) =
    let baseBitmask = bitmask |> replaceString "X" "0" |> binaryToInt64
    let baseTransformedAddress = address ||| baseBitmask
    let floatingBits =
        bitmask
        |> Seq.rev
        |> Seq.indexed
        |> Seq.filter (fun (_, c) -> c = 'X')
        |> Seq.map fst
    applyBitmaskOptions floatingBits baseTransformedAddress

let updateMemoryAtAllPositions (bitmask: string) (memory: Map<int64, int64>) (address: int64) (value: int64) =
    applyAllBitmaskOptionsToAddress bitmask address
    |> List.fold (fun mem address -> mem |> Map.add address value) memory

let instructionFolderB (bitmask: string, memory: Map<int64, int64>) line =
    let lineStart, value = line |> splitByString " = " |> unpack2
    match lineStart with
    | "mask" -> (value, memory)
    | ParseRegex @"mem\[([0-9]+)\]" [ Long address ] -> 
                (bitmask, updateMemoryAtAllPositions bitmask memory address (value |> int64))
    | _ -> failwithf "Invalid instruction line: %s" line

let solveB input =
    let (_, memory) =
        input
        |> Seq.fold instructionFolderB ("", Map.empty)
    memory
    |> Map.toList
    |> List.sumBy snd

let solve: string seq -> PartToSolve -> Unit = solveDay solveA solveB

let testInput = [
    "mask = 000000000000000000000000000000X1001X"
    "mem[42] = 100"
    "mask = 00000000000000000000000000000000X0XX"
    "mem[26] = 1"
]