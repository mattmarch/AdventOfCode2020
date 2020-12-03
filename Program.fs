﻿
let solveDay day part = 
  let solveFunction =
    match day with
    | "1" -> Day01.solve Day01.input
    | "2" -> Day02.solve Day02.input
    | "3" -> Day03.solve Day03.input
    | _ -> failwithf "Input doesn't seem to match any days solved days"
  solveFunction part


[<EntryPoint>]
let main argv =
  solveDay argv.[0] (Common.getPartFromArgs argv)
  0 // return an integer exit code
 