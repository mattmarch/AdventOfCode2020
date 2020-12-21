
let solveDay day part = 
  let solveFunction =
    match day with
    | "1" -> Day01.solve Day01.input
    | "2" -> Day02.solve Day02.input
    | "3" -> Day03.solve Day03.input
    | "4" -> Day04.solve Day04.input
    | "5" -> Day05.solve Day05.input
    | "6" -> Day06.solve Day06.input
    | "7" -> Day07.solve Day07.input
    | "8" -> Day08.solve Day08.input
    | "9" -> Day09.solve Day09.input
    | "10" -> Day10.solve Day10.input
    | "11" -> Day11.solve Day11.input
    | "12" -> Day12.solve Day12.input
    | "13" -> Day13.solve Day13.input
    | "14" -> Day14.solve Day14.input
    | "15" -> Day15.solve Day15.input
    | "16" -> Day16.solve Day16.input
    | "17" -> Day17b.solve Day17b.input
    | "18" -> Day18.solve Day18.input
    | "19" -> Day19.solve Day19.input
    | "20" -> Day20.solve Day20.input
    | _ -> failwithf "Input doesn't seem to match any days solved days"
  solveFunction part


[<EntryPoint>]
let main argv =
  solveDay argv.[0] (Common.getPartFromArgs argv)
  0 // return an integer exit code
 