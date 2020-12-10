module Day10

open Common

let input = readLines "Day10.txt" |> Seq.map int |> Seq.toList


let getDifferences =
    List.pairwise
    >> List.map (fun (a, b) -> b - a)

let countValue value =
    List.filter (fun x -> x = value)
    >> List.length

let solveA input = 
    let outletJolts = 0
    let builtInAdapterJolts = List.max input + 3
    let fullSortedJoltList = 
        input @ [outletJolts; builtInAdapterJolts]
        |> List.sort

    let joltDifferences =
        fullSortedJoltList
        |> getDifferences
    (countValue 1 joltDifferences) * (countValue 3 joltDifferences)

type AdapterValueState = {
    Value: int;
    WayCount: int64;
}

type PreviousAdaptersState = AdapterValueState list

let countPathsFolder (state: PreviousAdaptersState) nextAdapter =
    let pathsToAdapter =
        state
        |> List.filter (fun adapter -> adapter.Value >= nextAdapter - 3)
        |> List.sumBy (fun adapter -> adapter.WayCount)
    { Value = nextAdapter; WayCount = pathsToAdapter } :: state

let solveB input =
    let builtInAdapterJolts = List.max input + 3
    let sortedJoltList = 
        input @ [builtInAdapterJolts]
        |> List.sort

    let pathCount =
        sortedJoltList
        |> List.fold countPathsFolder [{ Value = 0; WayCount = 1L }]
        |> List.maxBy (fun adapter -> adapter.Value)
    pathCount.WayCount

let solve = solveDay solveA solveB

let testInput = [
    28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 
    38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3; ]