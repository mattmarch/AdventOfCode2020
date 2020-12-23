module Day23

open Common

let input = "538914762" |> Seq.toList |> List.map (string >> int)

let findDestinationCup cupsToSearch currentCupValue =
  let indexedCups = cupsToSearch |> List.indexed
  match indexedCups |> List.filter (fun (_, c) -> c < currentCupValue) with
  | [] -> indexedCups |> List.maxBy snd
  | smallerValueCups -> smallerValueCups |> List.maxBy snd

let applyMove cups =
  let currentCup = cups |> List.head
  let cupsToMove = cups |> List.tail |> List.take 3
  let remainingCups = cups |> List.skip 4
  let destinationIndex, _ = findDestinationCup remainingCups currentCup
  remainingCups.[..destinationIndex] @ cupsToMove @ remainingCups.[(destinationIndex+1)..] @ [currentCup]
  
let applyXMoves numberMoves cups =
  List.init numberMoves id
  |> List.fold (fun previousState _ -> applyMove previousState) cups

let solveA input =
  let finalResult = applyXMoves 100 input
  let indexOf1 = finalResult |> List.findIndex (fun cup -> cup = 1)
  (finalResult |> List.skip (indexOf1 + 1)) @ (finalResult |> List.take indexOf1)
  |> List.map string
  |> joinStrings

let solveB input =
  let allCups = input @ (List.init 1000000 id |> List.skip 10)
  let finalResult = applyXMoves 10000000 allCups
  let indexOf1 = finalResult |> List.findIndex (fun cup -> cup = 1)
  (int64 finalResult.[(indexOf1 + 1) % 1000000]) * (int64 finalResult.[(indexOf1 + 2) % 1000000])

let solve = solveDay solveA solveB

let testInput = "389125467" |> Seq.toList |> List.map (string >> int)