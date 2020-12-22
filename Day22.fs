module Day22

open Common

let input = readAll "Day22.txt"

let parseInput input =
  input
  |> splitByString "\n\n"
  |> List.map (splitBy '\n' >> List.tail >> List.map int)
  |> unpack2

let rec playGame (hand1: int list) (hand2: int list) =
  match List.tryHead hand1, List.tryHead hand2 with
  | Some card1, Some card2 when card1 > card2 ->
      playGame ((List.tail hand1) @ [card1; card2]) (List.tail hand2)
  | Some card1, Some card2 when card2 > card1 ->
      playGame (List.tail hand1) ((List.tail hand2) @ [card2; card1])
  | Some _, None -> hand1
  | None, Some _ -> hand2
  | Some card1, Some card2 -> failwithf "Unexpected identical cards found %i %i" card1 card2
  | None, None -> failwithf "Both hands are empty.. something has gone wrong!"

let solveA input =
  let player1Hand, player2Hand = parseInput input
  playGame player1Hand player2Hand
  |> List.rev
  |> List.indexed
  |> List.sumBy (fun (i, card) -> (i + 1) * card)

type Player = P1 | P2

let rec playRecursiveGame previousHands (hand1: int list) (hand2: int list) =
  if previousHands |> List.contains (hand1, hand2) then
    P1, hand1
  else
    let nextPreviousHands = (hand1, hand2) :: previousHands
    match List.tryHead hand1, List.tryHead hand2 with
    | Some card1, Some card2 ->
        if List.length hand1 > card1 && List.length hand2 > card2 then
          match playRecursiveGame [] (hand1 |> List.tail |> List.take card1) (hand2 |> List.tail |> List.take card2) with
          | P1, _ -> playRecursiveGame nextPreviousHands ((List.tail hand1) @ [card1; card2]) (List.tail hand2) 
          | P2, _ -> playRecursiveGame nextPreviousHands (List.tail hand1) ((List.tail hand2) @ [card2; card1])
        elif card1 > card2 then 
          playRecursiveGame nextPreviousHands ((List.tail hand1) @ [card1; card2]) (List.tail hand2)
        else
          playRecursiveGame nextPreviousHands (List.tail hand1) ((List.tail hand2) @ [card2; card1])
    | Some _, None -> P1, hand1
    | None, Some _ -> P2, hand2
    | None, None -> failwithf "Both hands are empty.. something has gone wrong!"

let solveB input =
  let player1Hand, player2Hand = parseInput input
  playRecursiveGame [] player1Hand player2Hand
  |> snd
  |> List.rev
  |> List.indexed
  |> List.sumBy (fun (i, card) -> (i + 1) * card)

let solve = solveDay solveA solveB

let testInput = 
  "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"