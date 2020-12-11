module Day11

open Common

let input = readLines "Day11.txt"

let parseInputLine (line: string) =
    line
    |> Seq.indexed
    |> Seq.filter (fun (_, c) -> c = 'L')
    |> Seq.map fst

let parseInput (input: string seq) =
    input
    |> Seq.map parseInputLine
    |> Seq.indexed
    |> Seq.collect (
        fun (yIndex, line) -> line 
                                |> Seq.map (fun xIndex -> ((xIndex, yIndex), false)))
    |> Map

let isSeatAndTaken (seats: Map<int*int, bool>) seatCoord =
    match seats.TryFind seatCoord with
    | Some true -> true
    | Some false -> false
    | None -> false

let surrounding = 
    [(-1, -1); (0, -1); (1, -1); (-1, 0); 
    (1, 0); (-1, 1); (0, 1); (1, 1)]

let countTakenSurroundingSeats seats (seatCoordx, seatCoordy) =
    surrounding
    |> List.map (fun (x, y) -> (seatCoordx + x, seatCoordy + y))
    |> List.filter (isSeatAndTaken seats)
    |> List.length

let seatShouldBeTaken (seats: Map<int*int, bool>) seat =
    let currentSeatIsTaken = seats |> Map.find seat
    let takenSurroundingSeats = countTakenSurroundingSeats seats seat
    match currentSeatIsTaken, takenSurroundingSeats with
    | false, 0 -> true
    | true, takenSeats when takenSeats >= 4 -> false
    | _ -> currentSeatIsTaken

let getNextSeatsState seats =
    let allSeats = seats |> Map.toSeq |> Seq.map fst
    allSeats
    |> Seq.map (fun seat -> (seat, seatShouldBeTaken seats seat))
    |> Map

let seatUnfolder stateGenerator previousState =
    let nextState = stateGenerator previousState
    if nextState = previousState then 
        None 
    else 
        Some (nextState, nextState)

let solveA input =
    let seats = parseInput input
    seats
    |> Seq.unfold (seatUnfolder getNextSeatsState)
    |> Seq.last
    |> Map.toSeq
    |> Seq.filter snd
    |> Seq.length

let getSeatLimits seats =
    let coords = 
        seats
        |> Map.toSeq
        |> Seq.map fst
    (
        coords |> Seq.map fst |> Seq.max,
        coords |> Seq.map snd |> Seq.max
    )

let seatIsOutsideLimits (seatX, seatY) (limitX, limitY) =
    seatX < 0 || seatY < 0 || seatX > limitX || seatY > limitY
    
let rec isSeatInDirectionTaken seats (fromSeatX, fromSeatY) limits (directionX, directionY) =
    let nextSeatInDirection = fromSeatX + directionX, fromSeatY + directionY
    match seats |> Map.tryFind nextSeatInDirection with
    | Some true -> true
    | Some false -> false
    | None when seatIsOutsideLimits nextSeatInDirection limits -> false
    | _ -> isSeatInDirectionTaken seats nextSeatInDirection limits (directionX, directionY)

let countTakenSeatsInDirections seats seatCoord limits =
    surrounding
    |> List.filter (isSeatInDirectionTaken seats seatCoord limits)
    |> List.length

let seatShouldBeTakenB (seats: Map<int*int, bool>) limits seat =
    let currentSeatIsTaken = seats |> Map.find seat
    let takenSeatsInDirections = countTakenSeatsInDirections seats seat limits
    match currentSeatIsTaken, takenSeatsInDirections with
    | false, 0 -> true
    | true, takenSeats when takenSeats >= 5 -> false
    | _ -> currentSeatIsTaken

let getNextSeatsStateB seats =
    let allSeats = seats |> Map.toSeq |> Seq.map fst
    let seatLimits = getSeatLimits seats
    allSeats
    |> Seq.map (fun seat -> (seat, seatShouldBeTakenB seats seatLimits seat))
    |> Map

let solveB input =
    let seats = parseInput input
    seats
    |> Seq.unfold (seatUnfolder getNextSeatsStateB)
    |> Seq.last
    |> Map.toSeq
    |> Seq.filter snd
    |> Seq.length

let solve: string seq -> PartToSolve -> Unit = 
    solveDay solveA solveB

let testInput = [
    "L.LL.LL.LL"
    "LLLLLLL.LL"
    "L.L.L..L.."
    "LLLL.LL.LL"
    "L.LL.LL.LL"
    "L.LLLLL.LL"
    "..L.L....."
    "LLLLLLLLLL"
    "L.LLLLLL.L"
    "L.LLLLL.LL"
]

let printOutput (sizeX, sizeY) seats =
    let xCoords = [0 .. sizeX]
    let yCoords = [0 .. sizeY]

    let getLine yCoord =
        xCoords
        |> List.map (fun x ->
            match seats |> Map.tryFind (x, yCoord) with
            | Some true -> '#'
            | Some false -> 'L'
            | None -> '.'
            )
        |> Array.ofList
        |> System.String

    yCoords
    |> List.map (getLine)
    |> List.iter (fun line -> printfn "%s" line)

let printTestOutput = printOutput (9, 9)