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

let validateHeight (height: string) =
    let unitStartPos = Seq.length height - 2
    let unit = height.[unitStartPos ..]
    let value = height.[.. unitStartPos - 1]
    match unit with
    | "cm" -> value |> int |> valueInRange (150, 193) 
    | "in" -> value |> int |> valueInRange (59, 76)
    | _ -> false

let isValidColourDigit (c: char) =
    let charAsInt = int c
    valueInRange (48, 57) charAsInt || valueInRange (97, 102) charAsInt

let validateHairColour (hairColour: string) =
    Seq.head hairColour = '#'
    && Seq.length hairColour = 7 
    && Seq.tail hairColour |> Seq.forall isValidColourDigit

let validEyeColours = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let validatePassportField (name, value) =
    match name with
    | "byr" -> value |> int |> valueInRange (1920, 2002)
    | "iyr" -> value |> int |> valueInRange (2010, 2020)
    | "eyr" -> value |> int |> valueInRange (2020, 2030)
    | "hgt" -> validateHeight value
    | "hcl" -> validateHairColour value
    | "ecl" -> validEyeColours |> List.contains value
    | "pid" -> Seq.length value = 9 && isNumber value
    | _ -> true

let solveB =
    List.map parseLine
    >> List.filter hasRequiredFields
    >> List.filter (List.forall validatePassportField)
    >> List.length

let solve = solveDay solveA solveB