module Common

open System
open System.IO
open System.Text.RegularExpressions

type PartToSolve = A | B | Both

let readLines path: string seq = File.ReadLines(path)
let readAll path = File.ReadAllText(path)

let splitBy (separator: char) (inputString: string): string list = 
  inputString.Split [|separator|] |> Array.toList

let splitByString (separator: string) (inputString: string): string list = 
  inputString.Split([|separator|], StringSplitOptions.None) |> Array.toList

let replaceString (beforeSubstr: string) (afterSubstr: string) (input: string) =
  input.Replace(beforeSubstr, afterSubstr)

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

let unpack3 l = 
  match l with
  | [a; b; c] -> a, b, c
  | _ -> failwithf "Tried to upack3 list without exactly 3 elements %A" l

let bigintProductOfInts: int list -> bigint =
  List.map bigint >> List.reduce (*)

let isNumber (s: string) =
    Int32.TryParse s |> fst
    
let valueInRange (rangeStart, rangeEnd) value =
    value >= rangeStart && value <= rangeEnd

let binaryToInt input = Convert.ToInt32(input, 2)

let binaryToInt64 input = Convert.ToInt64(input, 2)

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Integer|_|) (str: string) =
  match Int32.TryParse str with
  | true, value -> Some value
  | false, _ -> None

let (|Long|_|) (str: string) =
  match Int64.TryParse str with
  | true, value -> Some value
  | false, _ -> None

let positiveModulo value divisor = 
  match value % divisor with
  | positiveResult when positiveResult >= 0 -> positiveResult
  | negativeResult -> divisor + negativeResult

let positiveModuloInt64 value divisor = 
  match value % divisor with
  | positiveResult when positiveResult >= 0L -> positiveResult
  | negativeResult -> divisor + negativeResult

let allTriples l1 l2 l3 =
    List.allPairs l2 l3
    |> List.allPairs l1
    |> List.map (fun (a, (b, c)) -> a, b, c)

let joinStrings (strings: string list) = String.Concat(strings)

let joinChars (chars: char seq) = String.Concat(chars)

let stringContains (substring: string) (inputString: string) = 
  inputString.Contains(substring)

let tryStringIndexOf (substring: string) (inputString: string) =
  match inputString.IndexOf(substring) with
  | -1 -> None
  | positiveValue -> Some positiveValue

let tryRemovePrefix (message: string) (prefix: string) =
  if message.StartsWith prefix then
    Some (message.Substring(prefix.Length))
  else
    None

let duplicate v = v, v

let add2d (ax, ay) (bx, by) =
  (ax + bx), (ay + by)