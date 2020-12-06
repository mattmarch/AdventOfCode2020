module Day06

open Common

let input = readAll "Day06.txt" |> splitByString "\n\n"

let parseUniqueAnswersInGroup = 
  replaceString "\n" "" 
  >> Seq.distinct

let solveA (input: string list) =
  input
  |> List.map parseUniqueAnswersInGroup
  |> List.sumBy Seq.length


let parseEntireGroup =
  splitBy '\n' >> List.filter (fun l -> l <> "")

let countAnswersForAllInGroup: string list -> int =
  List.map Set.ofSeq
  >> Set.intersectMany
  >> Set.count

let solveB (input: string list) =
  input
  |> List.map parseEntireGroup
  |> List.sumBy countAnswersForAllInGroup

let solve = solveDay solveA solveB