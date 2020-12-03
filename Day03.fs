module Day03

open Common

let input = readLines "Day03.txt"

let getXthCharacter (x: int) (line: string) =
    line.[x % line.Length]

let charIsTree c =
    match c with
    | '.' -> false
    | '#' -> true
    | _ -> failwithf "Invalid character in input"

let solveSlope input (rightBy, downBy) =
    input
    |> Seq.indexed
    |> Seq.filter (fun (i, _) -> i % downBy = 0)
    |> Seq.map (fun (i, row) -> getXthCharacter (rightBy * i / downBy) row)
    |> Seq.filter charIsTree
    |> Seq.length

let solveA input =
    solveSlope input (3, 1)

let routes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]

let solveB input =
    routes
    |> List.map (solveSlope input)
    |> bigintProductOfInts

let solve: (string seq -> PartToSolve -> unit) = 
    solveDay solveA solveB

let testInput = seq {
    "..##.......";
    "#...#...#..";
    ".#....#..#.";
    "..#.#...#.#";
    ".#...##..#.";
    "..#.##.....";
    ".#.#.#....#";
    ".#........#";
    "#.##...#...";
    "#...##....#";
    ".#..#...#.#";
}