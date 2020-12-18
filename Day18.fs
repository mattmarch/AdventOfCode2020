module Day18

open Common

type Token =
    | Number of int
    | Add
    | Multiply
    | OpenParen
    | CloseParen

type TokenOrNestedTokens =
    | Token of Token
    | NestedTokens of TokenOrNestedTokens list

let input = readLines "Day18.txt"

let parseCharacter (c: char) =
    match string c with
    | Integer i -> Number i
    | "+" -> Add
    | "*" -> Multiply
    | "(" -> OpenParen
    | ")" -> CloseParen
    | _ -> failwithf "Unexpected character %c in input" c

let parseLine =
    replaceString " " ""
    >> Seq.map parseCharacter
    >> Seq.toList

let parseInput: string seq -> Token list seq = Seq.map parseLine

let rec nestExpressionsFolder nestedSoFar (token: Token) =
    match List.tryHead nestedSoFar, token with
    | Some (NestedTokens nestedTokens), _ when List.tryHead nestedTokens <> Some(Token CloseParen) ->
                NestedTokens (nestExpressionsFolder nestedTokens token) :: List.tail nestedSoFar
    | _, OpenParen -> NestedTokens [ Token OpenParen ] :: nestedSoFar
    | _, otherToken -> Token otherToken :: nestedSoFar

type Action =
    | Start
    | Addition
    | Multiplication
    | None

let initialSolveExpressionFolderState = (0, Start)

let rec solveExpressionFolder (total, action) (nextToken: TokenOrNestedTokens) =
    let unnestedNext: Token =
        match nextToken with
        | NestedTokens nested ->
            Number
                (nested
                 |> List.rev
                 |> List.fold solveExpressionFolder initialSolveExpressionFolderState
                 |> fst)
        | Token t -> t

    match action, unnestedNext with
    | Start, Number n -> (n, None)
    | Addition, Number n -> (total + n, None)
    | Multiplication, Number n -> (total * n, None)
    | None, Add -> (total, Addition)
    | None, Multiply -> (total, Multiplication)
    | action, OpenParen -> (total, action)
    | action, CloseParen -> (total, action)
    | _ -> failwithf "Unexpected token combinations %A" (action, unnestedNext)


let solveExpression (expression: Token list) =
    expression
    |> List.fold nestExpressionsFolder []
    |> List.rev
    |> List.fold solveExpressionFolder initialSolveExpressionFolderState
    |> fst

let solveA input =
    input
    |> parseInput
    |> Seq.sumBy (solveExpression >> int64)

let solveB input = ""

let solve input = solveDay solveA solveB input

let exampleInput =
    seq [ "1 + (2 * 3) + (4 * (5 + 6))"
          "2 * 3 + (4 * 5)"
          "5 + (8 * 3 + 9 + 3 * 4 * 3)"
          "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
          "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 
          ]
