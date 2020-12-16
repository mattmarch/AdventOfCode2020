module Day16

open Common

type ValueRange = int * int
type ValueRanges = ValueRange * ValueRange

type FieldRule = {
    Name: string;
    ValueRanges: ValueRanges
}

type TicketDetails = int list

type InputInformation = {
    RulesForFields: FieldRule list
    MyTicket: TicketDetails
    OtherTickets: TicketDetails list
}

let input = readAll "Day16.txt"

let fieldRuleRegex = @"([a-z| ]+): (\d+)-(\d+) or (\d+)-(\d+)"

let parseFieldRuleString line =
    match line with
    | ParseRegex fieldRuleRegex [name; Integer r1s; Integer r1e; Integer r2s; Integer r2e] ->
        {
            Name = name
            ValueRanges = ((r1s, r1e), (r2s, r2e))
        }
    | _ -> failwithf "Invalid field rule line %s " line

let parseInputFieldRules (inputFieldRules: string): FieldRule list =
    inputFieldRules 
    |> splitByString "\r\n"
    |> List.map parseFieldRuleString

let parseTicket line =
    line
    |> splitBy ','
    |> List.map int

let parseMyTicket inputMyTicket =
    inputMyTicket
    |> splitByString "\r\n"
    |> unpack2
    |> snd
    |> parseTicket

let parseOtherTickets inputOtherTickets =
    inputOtherTickets
    |> splitByString "\r\n"
    |> List.tail
    |> List.map parseTicket

let parseInput input =
    let inputFieldRules, inputMyTicket, inputOtherTickets =
        input 
        |> splitByString "\r\n\r\n" 
        |> unpack3
    {
        RulesForFields = parseInputFieldRules inputFieldRules;
        MyTicket = parseMyTicket inputMyTicket
        OtherTickets = parseOtherTickets inputOtherTickets
    }

let valueValidForField value fieldRule =
    let range1, range2 = fieldRule.ValueRanges
    valueInRange range1 value || valueInRange range2 value

let valueValidForAnyFields rulesForFields value =
    rulesForFields
    |> List.exists (valueValidForField value)

let getErrorRateForTicket rulesForFields (ticket: TicketDetails) =
    ticket
    |> List.filter (valueValidForAnyFields rulesForFields >> not)
    |> List.sum

let solveA input =
    let parsedInput = parseInput input
    parsedInput.OtherTickets
    |> List.sumBy (getErrorRateForTicket parsedInput.RulesForFields)

let ticketIsValid rulesForFields (ticket: TicketDetails) =
    ticket
    |> List.forall (valueValidForAnyFields rulesForFields)

let getIndexesRuleIsValidFor rule ticket =
    ticket
    |> List.indexed
    |> List.filter (fun (_, v) -> valueValidForField v rule)
    |> List.map fst

let getIndexesRuleIsValidForInAllTickets tickets rule =
    tickets
    |> List.map (getIndexesRuleIsValidFor rule >> Set.ofList)
    |> Set.intersectMany
    |> Set.toList

let removeValuesFromList valuesToRemove =
    List.filter (fun x -> not (List.contains x valuesToRemove))

let ignoreIndicesChooser indicesToIgnore (rule, possibleIndices: int list) =
    match possibleIndices |> removeValuesFromList indicesToIgnore with
    | [] -> None
    | indicesWithSomeIgnored -> Some (rule, indicesWithSomeIgnored)

let rec matchRulesToIndices (rulesAndPossibleIndices: (FieldRule * int list) list) indicesToIgnore =
    let rulesSortedByMatchingIndices =
        rulesAndPossibleIndices
        |> List.choose (ignoreIndicesChooser indicesToIgnore)
        |> List.sortBy (fun (_, indicesList) -> List.length indicesList)
    match List.tryHead rulesSortedByMatchingIndices with
    | Some (rule, [singleIndex]) -> 
        (rule, singleIndex) :: (matchRulesToIndices rulesAndPossibleIndices (singleIndex :: indicesToIgnore))
    | None -> []
    | Some ruleWithMultipleIndices ->
        failwithf "Not clever enough to handle multiple possibilities %A" ruleWithMultipleIndices

let ruleBeginsWithDeparture rule =
    (rule.Name |> splitBy ' ' |> List.head) = "departure" 

let solveB input =
    let parsedInput = parseInput input
    let validTickets =
        parsedInput.OtherTickets
        |> List.filter (ticketIsValid parsedInput.RulesForFields)
    let rulesAndPossibleIndices =
        parsedInput.RulesForFields
        |> List.map (fun r -> r, getIndexesRuleIsValidForInAllTickets validTickets r)
    let rulesMatchedWithIndices = matchRulesToIndices rulesAndPossibleIndices []
    rulesMatchedWithIndices
    |> List.filter (fst >> ruleBeginsWithDeparture)
    |> List.map (fun (_, i) -> List.item i parsedInput.MyTicket)
    |> bigintProductOfInts

let solve = solveDay solveA solveB