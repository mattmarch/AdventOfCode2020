module Day01

open Common

let input = readLines "Day01.txt" |> Seq.map int |> Seq.toList

let targetValue = 2020

let findSumPair target input =
    List.allPairs input input
    |> List.find (fun (a, b) -> a + b = target)

let multiplyPair (a, b) = a * b

let solveA = findSumPair targetValue >> multiplyPair

let findSumTriple target input =
    allTriples input input input
    |> List.find (fun (a, b, c) -> a + b + c = target)

let multiplyTriple (a, b, c) = a * b * c

let solveB = findSumTriple targetValue >> multiplyTriple

let solve = solveDay solveA solveB