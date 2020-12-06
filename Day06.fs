module Day06

open Common

let input = readAll "Day06.txt" |> splitByString "\n\n"

let parseGroup = 
  replaceString "\n" "" 
  >> Seq.distinct

let solveA (input: string list) =
  input
  |> List.map parseGroup
  |> List.sumBy Seq.length


let parseGroupB =
  splitBy '\n' >> List.filter (fun l -> l <> "")

let countGroupsInAll: string list -> int =
  List.map Set.ofSeq
  >> Set.intersectMany
  >> Set.count

let solveB (input: string list) =
  input
  |> List.map parseGroupB
  |> List.sumBy countGroupsInAll

let solve = solveDay solveA solveB