module Common

open System.IO

type PartToSolve = A | B | Both

let readLines path: string seq = File.ReadLines(path)
let readAll path = File.ReadAllText(path)

let splitBy (separator: char) (inputString: string): string list = 
  inputString.Split [|separator|] |> Array.toList

let toLower (str: string) = str.ToLower()

let solveDay partA partB input partToSolve = 
  if partToSolve <> B then printfn "Part A: %A" (partA input)
  if partToSolve <> A then printfn "Part B: %A" (partB input)

let getPartFromArgs arguments =
    if Array.length arguments <= 1 
    then Both
    else match arguments |> (Array.map toLower) with
            | args when Array.contains "a" args -> A
            | args when Array.contains "b" args -> B
            | _ -> Both
    
let unpack2 l =
  match l with
  | [a; b] -> a, b
  | _ -> failwithf "Tried to unpack2 list without exactly 2 elements: %A" l