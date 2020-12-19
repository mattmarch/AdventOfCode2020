module Day19

open Common

type SubRule = int list

type Rule =
  | SingleCharacter of char
  | SubRules of SubRule list

type RuleSet = Map<int, Rule>

let input = 
  readAll "Day19.txt" 
  |> splitByString "\n\n" 
  |> List.map (splitBy '\n')
  |> unpack2

let parseSubrules subRules =
  subRules
  |> splitByString " | "
  |> List.map (splitBy ' ' >> List.map int)

let parseRule (ruleString: string) =
  match ruleString with
  | ParseRegex @"(\d+): \u0022([a-z])\u0022" [Integer ruleNumber; character] -> 
      (ruleNumber, SingleCharacter (Seq.head character))
  | ParseRegex @"(\d+): ((?:\d+|[ ]|\|)+)" [Integer ruleNumber; subRules] ->
      (ruleNumber, SubRules (parseSubrules subRules))
  | _ -> failwithf "Unmatched input line: %s" ruleString

let parseRules rules: RuleSet =
  rules
  |> List.map parseRule
  |> Map

let joinStringMessagePartOptions (options1: string list) (options2: string list) =
  List.allPairs options1 options2
  |> List.map (fun (a, b) -> a + b)

let rec generateValidMessagesForRule (ruleSet: RuleSet) ruleNumber: string list =
  match ruleSet |> Map.find ruleNumber with
  | SingleCharacter c -> [string c]
  | SubRules subRules -> 
      subRules 
      |> List.collect (fun subRules ->
                          subRules
                          |> List.map (generateValidMessagesForRule ruleSet)
                          |> List.reduce joinStringMessagePartOptions
                          )

let solveA input =
  let rulesSet = input |> fst |> parseRules
  let allValidMessages = generateValidMessagesForRule rulesSet 0
  input
  |> snd
  |> List.filter (fun message -> List.contains message allValidMessages)
  |> List.length

let testInput = (
  ["0: 4 1 5";
  "1: 2 3 | 3 2";
  "2: 4 4 | 5 5";
  "3: 4 5 | 5 4";
  "4: \"a\"";
  "5: \"b\""],

  ["ababbb";
  "bababa";
  "abbbab";
  "aaabbb";
  "aaaabbb"]
)