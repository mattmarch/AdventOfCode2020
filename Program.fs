
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
    | _ -> failwithf "Input doesn't seem to match any days solved days"
  solveFunction part


[<EntryPoint>]
let main argv =
  solveDay argv.[0] (Common.getPartFromArgs argv)
  0 // return an integer exit code
 