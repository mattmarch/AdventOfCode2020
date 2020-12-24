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

let findNextInsertionPoint cup removedCups =
  let rec decreaseBy1 previousValue =
    let target = (positiveModulo (previousValue - 2) 1000000) + 1
    if removedCups |> List.contains target then
      decreaseBy1 target
    else
      target
  decreaseBy1 cup

let applyMoveToMap currentCup cups =
  let firstCup = cups |> Map.find currentCup
  let secondCup = cups |> Map.find firstCup
  let thirdCup = cups |> Map.find secondCup
  let endOfRemovedCups = cups |> Map.find thirdCup
  let insertionPoint = findNextInsertionPoint currentCup [firstCup; secondCup; thirdCup]
  (
    endOfRemovedCups,
    cups 
    |> Map.add currentCup endOfRemovedCups // close the gap
    |> Map.add insertionPoint firstCup // start of new position
    |> Map.add thirdCup (cups |> Map.find insertionPoint) // end of new position
  )

let applyXMovesToMap numberMoves startCup cups =
  List.init numberMoves id
  |> List.fold (fun (nextCup, previousCups) _ -> applyMoveToMap nextCup previousCups) (startCup, cups)
  |> snd

let solveB input =
  let allCups = 
    (input |> List.pairwise) 
    @ [(List.last input, 10)] 
    @ (List.init 1000000 (fun i -> (i, i+1)) |> List.skip 10)
    @ [(1000000, List.head input)]
  |> Map.ofList
  |> applyXMovesToMap 10000000 (List.head input)
  let firstStarCup = allCups |> Map.find 1
  let secondStarCup = allCups |> Map.find firstStarCup
  (int64 firstStarCup) * (int64 secondStarCup)

let solve = solveDay solveA solveB

let testInput = "389125467" |> Seq.toList |> List.map (string >> int)