module Day12

open Common
open System

type DegreeTurn = 
  | Ninety
  | OneEighty
  | TwoSeventy

type Action =
  | North of int
  | South of int
  | East of int
  | West of int
  | Left of DegreeTurn
  | Right of DegreeTurn
  | Forward of int

type State = {
  X: int
  Y: int
  CWTurnsFromEast: int
}

let input = readLines "Day12.txt"

let splitAction (action: string) =
  (Seq.head action, Seq.tail action |> String.Concat)

let (|MatchDegrees|_|) degreeString =
  match degreeString with
  | "90" -> Some Ninety
  | "180" -> Some OneEighty
  | "270" -> Some TwoSeventy
  | _ -> None

let degreeTurnToQuarterTurns degreeTurn =
  match degreeTurn with
  | Ninety -> 1
  | OneEighty -> 2
  | TwoSeventy -> 3

let parseLine (line: string) =
  match splitAction line with
  | 'N', Integer i -> North i
  | 'S', Integer i -> South i
  | 'E', Integer i -> East i
  | 'W', Integer i -> West i
  | 'L', MatchDegrees deg -> Left deg
  | 'R', MatchDegrees deg -> Right deg
  | 'F', Integer i -> Forward i
  | _ -> failwithf "Invalid action %s" line

let moveStateForward state amount =
  match positiveModulo state.CWTurnsFromEast 4 with
  | 0 -> {state with X = state.X + amount}
  | 1 -> {state with Y = state.Y - amount}
  | 2 -> {state with X = state.X - amount}
  | 3 -> {state with Y = state.Y + amount}
  | _ -> failwithf "Something has gone very wrong with the modulo operator"

let updateState state action =
  match action with
  | North i -> {state with Y = state.Y + i}
  | South i -> {state with Y = state.Y - i}
  | East i -> {state with X = state.X + i}
  | West i -> {state with X = state.X - i}
  | Left deg -> {state with CWTurnsFromEast = state.CWTurnsFromEast - degreeTurnToQuarterTurns deg}
  | Right deg -> {state with CWTurnsFromEast = state.CWTurnsFromEast + degreeTurnToQuarterTurns deg}
  | Forward i -> moveStateForward state i

let manhattanDistance state = abs state.X + abs state.Y

let initialState = { X = 0; Y = 0; CWTurnsFromEast = 0 }

let solveA input =
  input
  |> Seq.map parseLine
  |> Seq.fold updateState initialState
  |> manhattanDistance

type WaypointState = {
  WaypointX: int;
  WaypointY: int;
  ShipX: int;
  ShipY: int;
}

type TurnDirection = RightRotation | LeftRotation

let rotateState state degreeTurn turnDirection =
  match degreeTurn, turnDirection with
  | (Ninety, RightRotation) | (TwoSeventy, LeftRotation) 
    -> {state with WaypointX = state.WaypointY; WaypointY = -state.WaypointX}
  | (OneEighty, _)
    -> {state with WaypointX = -state.WaypointX; WaypointY = -state.WaypointY}
  | (TwoSeventy, RightRotation) | (Ninety, LeftRotation)
    -> {state with WaypointX = -state.WaypointY; WaypointY = state.WaypointX}

let moveTowardWaypoint state amount =
  {state with 
    ShipX = state.ShipX + (state.WaypointX * amount)
    ShipY = state.ShipY + (state.WaypointY * amount)
  }

let updateWaypointState (state: WaypointState) action =
  match action with
  | North i -> {state with WaypointY = state.WaypointY + i}
  | South i -> {state with WaypointY = state.WaypointY - i}
  | East i -> {state with WaypointX = state.WaypointX + i}
  | West i -> {state with WaypointX = state.WaypointX - i}
  | Left deg -> rotateState state deg LeftRotation
  | Right deg -> rotateState state deg RightRotation
  | Forward i -> moveTowardWaypoint state i

let initialWaypointState = { WaypointX = 10; WaypointY = 1; ShipX = 0; ShipY = 0 }

let waypointStateManhattanDistance state = abs state.ShipX + abs state.ShipY 

let solveB input =
  input
  |> Seq.map parseLine
  |> Seq.fold updateWaypointState initialWaypointState
  |> waypointStateManhattanDistance

let solve: string seq -> PartToSolve -> Unit = solveDay solveA solveB

let testInput = [
  "F10"
  "N3"
  "F7"
  "R90"
  "F11"
]
