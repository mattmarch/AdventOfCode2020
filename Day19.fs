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

let matchesRules42And31 rule42Possibilities rule31Possibilities message =
  let rec countOccurrencesAndReduceMessage possibilities message count =
    match possibilities |> List.tryPick (tryRemovePrefix message) with
    | Some reducedMessage -> countOccurrencesAndReduceMessage possibilities reducedMessage (count + 1)
    | None -> count, message
  let rule42Count, messageWithoutRule42 = countOccurrencesAndReduceMessage rule42Possibilities message 0
  let rule31Count, remainingMessage = countOccurrencesAndReduceMessage rule31Possibilities messageWithoutRule42 0
  rule31Count >= 1 && rule42Count > rule31Count && remainingMessage = ""

let solveB input =
  let rulesSet = input |> fst |> parseRules
  let rule42Possibilities = generateValidMessagesForRule rulesSet 42
  let rule31Possibilities = generateValidMessagesForRule rulesSet 31
  let messages = input |> snd
  messages
  |> List.filter (matchesRules42And31 rule42Possibilities rule31Possibilities)
  |> List.length
  
let solve = solveDay solveA solveB

let testInput1 = (
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

let testInput2 = (
  ["42: 9 14 | 10 1";
  "9: 14 27 | 1 26";
  "10: 23 14 | 28 1";
  "1: \"a\"";
  "11: 42 31";
  "5: 1 14 | 15 1";
  "19: 14 1 | 14 14";
  "12: 24 14 | 19 1";
  "16: 15 1 | 14 14";
  "31: 14 17 | 1 13";
  "6: 14 14 | 1 14";
  "2: 1 24 | 14 4";
  "0: 8 11";
  "13: 14 3 | 1 12";
  "15: 1 | 14";
  "17: 14 2 | 1 7";
  "23: 25 1 | 22 14";
  "28: 16 1";
  "4: 1 1";
  "20: 14 14 | 1 15";
  "3: 5 14 | 16 1";
  "27: 1 6 | 14 18";
  "14: \"b\"";
  "21: 14 1 | 1 14";
  "25: 1 1 | 1 14";
  "22: 14 14";
  "8: 42";
  "26: 14 22 | 1 20";
  "18: 15 15";
  "7: 14 5 | 1 21";
  "24: 14 1"],

  ["abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa";
  "bbabbbbaabaabba";
  "babbbbaabbbbbabbbbbbaabaaabaaa";
  "aaabbbbbbaaaabaababaabababbabaaabbababababaaa";
  "bbbbbbbaaaabbbbaaabbabaaa";
  "bbbababbbbaaaaaaaabbababaaababaabab";
  "ababaaaaaabaaab";
  "ababaaaaabbbaba";
  "baabbaaaabbaaaababbaababb";
  "abbbbabbbbaaaababbbbbbaaaababb";
  "aaaaabbaabaaaaababaa";
  "aaaabbaaaabbaaa";
  "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa";
  "babaaabbbaaabaababbaabababaaab";
  "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]
)