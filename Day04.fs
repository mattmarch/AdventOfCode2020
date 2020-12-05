module Day04

open Common

let input = readAll "Day04.txt" |> splitByString "\r\n\r\n"

let parseLine =
    replaceString "\r\n" " "
    >> splitBy ' '
    >> List.filter (fun item -> item <> "")
    >> List.map (splitBy ':' >> unpack2)

let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

let hasRequiredFields (passport: (string * string) list) =
    passport
    |> List.map fst
    |> List.filter (fun key -> requiredFields |> List.contains key)
    |> List.length = (requiredFields |> List.length)

let solveA =
    List.map parseLine
    >> List.filter hasRequiredFields
    >> List.length

let validEyeColours = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let validatePassportField (name, value) =
    match name, value with
    | "byr", Integer birthYear                          -> valueInRange (1920, 2002) birthYear
    | "iyr", Integer issueYear                          -> valueInRange (2010, 2020) issueYear
    | "eyr", Integer expiryYear                         -> valueInRange (2020, 2030) expiryYear
    | "hgt", ParseRegex @"(\d*)cm" [ Integer cm ]       -> cm >= 150 && cm <= 193
    | "hgt", ParseRegex @"(\d*)in" [ Integer inches ]   -> inches >= 59 && inches <= 76
    | "hcl", ParseRegex "#[a-f0-9]{6}" _                -> true
    | "ecl", eyeColour                                  -> List.contains eyeColour validEyeColours
    | "pid", ParseRegex "^[0-9]{9}$" _                  -> true
    | "cid", _                                          -> true
    | _                                                 -> false

let validatePassport = List.forall validatePassportField

let solveB =
    List.map parseLine
    >> List.filter hasRequiredFields
    >> List.filter validatePassport
    >> List.length

let solve = solveDay solveA solveB