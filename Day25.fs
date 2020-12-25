module Day25

open Common

let input = 15113849L, 4206373L

let applyLoop subjectNumber previousValue =
    positiveModuloInt64 (previousValue * subjectNumber) 20201227L

let getLoopResults subjectNumber =
    Seq.unfold (applyLoop subjectNumber >> duplicate >> Some) 1L

let solveA input =
    let cardPubKey, doorPubKey = input
    let loopResults = getLoopResults 7L
    let cardLoopNumber = (loopResults |> Seq.findIndex ((=) cardPubKey))
    getLoopResults doorPubKey |> Seq.item cardLoopNumber

let solve = solveDay solveA (fun x -> "No part b!")

let testInput = 5764801L, 17807724L