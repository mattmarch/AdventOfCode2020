module Day15

open Common

let input = [12;1;16;3;11;0]

let initialiseStartingNumbersMap (input: int list) =
    input
    |> List.indexed
    |> List.map (fun (i, value) -> value, i)
    |> Map

let separateLastElementAndIndex input =
    let lastElementIndex = List.length input - 1
    let lastElementValue = List.last input
    let precedingValues = input.[0..(lastElementIndex - 1)]
    precedingValues, lastElementValue, lastElementIndex

let getNumberAtIndex indexToFind startingNumbers =
    let allButLastStartingNumbers, lastStartingNumber, lastStartingNumberIndex = 
        separateLastElementAndIndex startingNumbers
    let rec getValuesRecursive (previousNumbers: Map<int, int>) lastIndex lastValue =
        let nextValue =
            match previousNumbers |> Map.tryFind lastValue with
            | Some position -> lastIndex - position
            | None -> 0
        match lastIndex + 1 with
        | nextIndex when nextIndex = indexToFind -> nextValue
        | nextIndex -> getValuesRecursive (previousNumbers |> Map.add lastValue lastIndex) nextIndex nextValue
    getValuesRecursive 
        (initialiseStartingNumbersMap allButLastStartingNumbers) 
        lastStartingNumberIndex 
        lastStartingNumber

let solveA = getNumberAtIndex (2020 - 1)

let solveB = getNumberAtIndex (30000000 - 1)

let solve: int list -> PartToSolve -> Unit = solveDay solveA solveB

let memoryGameUnfolder previousValues =
    let valueToFind = Seq.head previousValues
    let valuesToSearch = Seq.tail previousValues
    let nextValue = 
        match Seq.tryFindIndex ((=) valueToFind) valuesToSearch with
        | Some i -> i + 1
        | None -> 0
    Some (nextValue, Seq.append (Seq.singleton nextValue) previousValues)

let memoryGame (startingNumbers: int seq) = 
    Seq.append startingNumbers (Seq.unfold memoryGameUnfolder (Seq.rev startingNumbers))

let originalSolveA = List.toSeq >> memoryGame >> Seq.take 2020 >> Seq.last