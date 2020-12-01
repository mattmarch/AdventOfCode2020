module Day01

open Common

let input = readLines "Day01.txt" |> Seq.map int |> Seq.toList

let targetValue = 2020

let findSumPair inputList value =
    if List.contains (targetValue - value) inputList
    then Some (targetValue -  value, value)
    else None

let findPair input =
    input |> List.pick (findSumPair input)

let multiplyPair pair =
    printfn "%A" pair
    fst pair * snd pair

let solveA input = findPair input |> multiplyPair

let solveB input = "Solution to B"

let solve input = solveDay solveA solveB input