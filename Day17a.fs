module Day17a

open Common

type State = Active | Inactive

let input = readLines "Day17.txt" 

let matchInputChar inputChar =
    match inputChar with
    | '#' -> Active
    | '.' -> Inactive
    | _ -> failwithf "Invalid char in input: %c" inputChar

let parseInput (input: string seq) = 
    input
    |> Seq.indexed
    |> Seq.collect (fun (y, line) -> 
                    line 
                    |> (Seq.indexed >> Seq.map (fun (x, v) -> (x, y, 0), matchInputChar v)))
    |> Map.ofSeq

let getSurroundingCubes (coordX, coordY, coordZ) =
    let distances = [-1; 0; 1]
    allTriples distances distances distances
    |> List.filter ((<>) (0,0,0))
    |> List.map (fun (x, y, z) -> coordX + x, coordY + y, coordZ + z)

let getCubeState pocketState coord =
    match Map.tryFind coord pocketState with
    | Some Active -> Active
    | Some Inactive -> Inactive
    | None -> Inactive

let countActiveSurroundingCubes pocketState coord =
    coord
    |> getSurroundingCubes
    |> List.sumBy (fun neighbourCoord -> 
                    match getCubeState pocketState neighbourCoord with
                    | Active -> 1
                    | Inactive -> 0
                    )

let getNextCubeState pocketState coord =
    let currentState = getCubeState pocketState coord
    let activeNeighbourCount = countActiveSurroundingCubes pocketState coord
    match currentState, activeNeighbourCount with
    | Active, activeNeighbours when valueInRange (2, 3) activeNeighbours -> Active
    | Inactive, 3 -> Active
    | _ -> Inactive

let getCubesToCheck (xMin, xMax) (yMin, yMax) (zMin, zMax) =
    allTriples [xMin..xMax] [yMin..yMax] [zMin..zMax]

let runCubeCycle startState xRange yRange zRange =
    getCubesToCheck xRange yRange zRange
    |> Seq.map (fun coord -> coord, getNextCubeState startState coord)
    |> Map.ofSeq

let expand (rangeStart, rangeEnd) = (rangeStart - 1, rangeEnd + 1)

let rec applyCycles cyclesRemaining startState xRange yRange zRange =
    let nextState = runCubeCycle startState xRange yRange zRange
    if cyclesRemaining = 1 then
        nextState
    else
        applyCycles (cyclesRemaining - 1) nextState (expand xRange) (expand yRange) (expand zRange)

let solveA input =
    let inputLength = Seq.length input
    let initialState = parseInput input
    applyCycles 6 initialState (-1, inputLength) (-1, inputLength) (-1, 1)
    |> Map.toSeq
    |> Seq.sumBy (snd >> (fun state -> if state = Active then 1 else 0))
