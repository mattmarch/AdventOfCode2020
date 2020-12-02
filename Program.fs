
let solveDay day part = 
  match day with
    | "1" -> Day01.solve Day01.input part
    | "2" -> Day02.solve Day02.input part
    | _ -> printfn "Input doesn't seem to match any days solved days"


[<EntryPoint>]
let main argv =
  solveDay argv.[0] (Common.getPartFromArgs argv)
  0 // return an integer exit code
 