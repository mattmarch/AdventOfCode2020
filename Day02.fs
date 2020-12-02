module Day02

open Common

type Policy =
    { Letter: char
      MinOccurences: int
      MaxOccurences: int }

type PasswordAndPolicy = { Password: string; Policy: Policy }

let input = readLines "Day02.txt"

let parsePolicy policyString: Policy =
    let splitRangeAndLetter = policyString |> splitBy ' '
    let splitRange = splitRangeAndLetter.[0] |> splitBy '-'
    { Letter = splitRangeAndLetter.[1].[0]
      MinOccurences = splitRange.[0] |> int
      MaxOccurences = splitRange.[1] |> int }

let parseInputLine inputLine: PasswordAndPolicy =
    let splitByColon = inputLine |> splitBy ':'
    { Password = splitByColon.[1]
      Policy = parsePolicy splitByColon.[0] }

let isPasswordValid passwordAndPolicy =
    let occurences =
        passwordAndPolicy.Password
        |> Seq.filter (fun c -> c = passwordAndPolicy.Policy.Letter)
        |> Seq.length
    occurences >= passwordAndPolicy.Policy.MinOccurences
    && occurences <= passwordAndPolicy.Policy.MaxOccurences


let solveA =
    Seq.map parseInputLine
    >> Seq.filter isPasswordValid
    >> Seq.length

let xor a b = (a || b) && not (a && b)

let isPasswordValidPartB passwordAndPolicy =
    let positions = passwordAndPolicy.Policy.MinOccurences, passwordAndPolicy.Policy.MaxOccurences
    let charactersAtPositions =
        passwordAndPolicy.Password.[fst positions], passwordAndPolicy.Password.[snd positions]
    let targetCharacter = passwordAndPolicy.Policy.Letter
    xor (fst charactersAtPositions = targetCharacter) (snd charactersAtPositions = targetCharacter)

let solveB =
    Seq.map parseInputLine
    >> Seq.filter isPasswordValidPartB
    >> Seq.length

let solve: (string seq -> PartToSolve -> unit) = 
    solveDay solveA solveB


let testInput =
    [ "1-3 a: abcde";
      "1-3 b: cdefg";
      "2-9 c: ccccccccc" ] |> List.toSeq

