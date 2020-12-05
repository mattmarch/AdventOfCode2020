module Day05

open Common
open System

let input = readLines "Day05.txt"

let parseBinaryCode oneCode zeroCode =
  replaceString oneCode "1" 
  >> replaceString zeroCode "0"
  >> binaryToInt

let parseRow = parseBinaryCode "B" "F"
let parseColumn = parseBinaryCode "R" "L"

let parseLine (line: string) =
  let row = line.[..6] |> parseRow
  let column = line.[7..] |> parseColumn
  (row, column)

let calculateSeatId (row, column) = row * 8 + column

let solveA: string seq -> int =
  Seq.map (parseLine >> calculateSeatId)  
  >> Seq.max

let solveB: string seq -> int =
  Seq.map (parseLine)
  >> Seq.map calculateSeatId
  >> Seq.sort
  >> Seq.windowed 2
  >> Seq.map (Array.toList >> unpack2)
  >> Seq.pick (fun (a, b) -> if b - a = 2 then Some (a + 1) else None)



let solve: string seq -> PartToSolve -> Unit = solveDay solveA solveB