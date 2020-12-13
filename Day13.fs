module Day13

open Common

let input = 
  readLines "Day13.txt" 
  |> Seq.toList 
  |> unpack2
  |> (fun (timestamp, busIds) -> int timestamp, busIds |> splitBy ',')

let solveA (timestamp, busIds) =
  let busId, timeToWait =
    busIds
    |> List.filter (fun busId -> busId <> "x")
    |> List.map (int >> fun busId -> busId, busId - (timestamp % busId))
    |> List.minBy snd
  busId * timeToWait

// calculate z for z * y = 1 mod n
let invMod y n =
  Seq.initInfinite int64
  |> Seq.find (fun z -> (z * y) % n = 1L)

// Calculate solution to Chinese Remainder Theorem
// for a set of equations x = ai mod ni, input is a list of tuples (ai, ni)
// with product of all ni = N
// solution is sum of all ai * yi * zi modulo N
// where yi = N/ni
// and zi is found by zi * yi = 1 mod ni
let chineseRemainderTheoremSolver (input: (int64*int64) list) =
  let productN =
    input
    |> List.map snd
    |> List.reduce (*)
  let y =
    input
    |> List.map (fun (_, ni) -> productN / ni)
  let z =
    input
    |> List.zip y
    |> List.map (fun (yi, (_, ni)) -> invMod yi ni)
  let nonLowestSolution =
    input
    |> List.zip3 y z
    |> List.sumBy (fun (yi, zi, (ai, _)) -> ai * yi * zi)
  positiveModuloInt64 nonLowestSolution productN

let solveB (_, busIds) =
  busIds
  |> List.indexed
  |> List.filter (fun (_, busId) -> busId <> "x")
  |> List.map (fun (i, busId) -> - int64 i, int64 busId)
  |> chineseRemainderTheoremSolver

let solve = solveDay solveA solveB

let testInput = 939, ["7";"13";"x";"x";"59";"x";"31";"19"]

let testInput2 = 0, ["17"; "x"; "13"; "19"]

let wikipediaCrtExample = [(0L, 3L); (3L, 4L); (4L, 5L)]

let verify timestamp indexedBusIds = 
  indexedBusIds
  |> List.filter (fun (i, busId) -> (timestamp + i) % busId <> 0L)
