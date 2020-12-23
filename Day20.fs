module Day20

open Common

type Pixel = On | Off

type Side = Top | Right | Bottom | Left

type Tile = {
  Id: int
  Pixels: Pixel list list
}

let input = readAll "Day20.txt";

let parsePixel pixelChar =
  match pixelChar with
  | '#' -> On
  | '.' -> Off
  | invalid -> failwithf "Invalid Pixel %c" invalid

let parseTile tileString =
  let tileLines = tileString |> splitBy '\n'
  {
    Id =
      match List.head tileLines with
      | ParseRegex @"Tile (\d+):" [Integer id] -> id
      | invalid -> failwithf "Tile missing id line, instead was %s" invalid
    Pixels =
      tileLines
      |> List.tail
      |> List.map (Seq.map parsePixel >> Seq.toList)
  }

let parseInput input =
  input
  |> splitByString "\n\n"
  |> List.map parseTile

let rotateTile90CW tile =
  { tile with Pixels =
                tile.Pixels
                |> List.transpose
                |> List.map List.rev 
  }

let flipTileXAxis tile =
  { tile with Pixels = 
                tile.Pixels 
                |> List.map List.rev 
  }

let getTileEdge tile side =
  tile.Pixels
  |> match side with
      | Top -> List.head
      | Bottom -> List.last
      | Left -> List.map List.head
      | Right -> List.map List.last

let tryMatchTile edgePattern side tile =
  let allRotations =
    Seq.unfold (rotateTile90CW >> duplicate >> Some) tile
    |> Seq.take 4
  let allRotationsAndFlips =
    Seq.append allRotations (allRotations |> Seq.map flipTileXAxis)
  allRotationsAndFlips
  |> Seq.tryFind (fun transformedTile -> edgePattern = getTileEdge transformedTile side)

let findMatchingTile tileList edgePattern side =
  tileList
  |> List.tryPick (tryMatchTile edgePattern side)

type Neighbour = {
  Position: int * int
  RootSide: Side
  NeighbourSide: Side
}

let buildGrid (tileList: Tile list) =
  let firstTile = List.head tileList
  let initialPosition = (0, 0)
  let initialGrid = [initialPosition, firstTile] |> Map.ofList

  let rec placeNeighboursOfTile currentGrid (posX, posY) =
    let thisTile = currentGrid |> Map.find (posX, posY)
    let placedTileIds = currentGrid |> Map.toList |> List.map (fun (_, tile) -> tile.Id)
    let tilesStillToPlace = tileList |> List.filter (fun tile -> not(placedTileIds |> List.contains tile.Id))
    let takenPositions = currentGrid |> Map.toList |> List.map fst
    let neighboursToPlace =
      [
        { Position = (posX, posY + 1); RootSide = Top; NeighbourSide = Bottom }
        { Position = (posX, posY - 1); RootSide = Bottom; NeighbourSide = Top }
        { Position = (posX + 1, posY); RootSide = Right; NeighbourSide = Left }
        { Position = (posX - 1, posY); RootSide = Left; NeighbourSide = Right }
      ] 
      |> List.filter (fun neighbour -> not (takenPositions |> List.contains neighbour.Position))
    let gridWithNeighbours =
      neighboursToPlace
      |> List.fold (fun grid neighbour -> 
            match findMatchingTile tilesStillToPlace (getTileEdge thisTile neighbour.RootSide) neighbour.NeighbourSide with
            | Some tile -> 
                grid |> Map.add neighbour.Position tile
            | None -> grid
            ) currentGrid
    neighboursToPlace
    |> List.map (fun n -> n.Position)
    |> List.fold (fun grid position -> 
                    if grid |> Map.containsKey position then
                      placeNeighboursOfTile grid position
                    else
                      grid
    ) gridWithNeighbours
  placeNeighboursOfTile initialGrid initialPosition

let solveA input = 
  let tileList = parseInput input
  let tileGrid = buildGrid tileList
  let positions = 
    tileGrid
    |> Map.toList 
    |> List.map fst
  let minX = positions |> List.map fst |> List.min
  let maxX = positions |> List.map fst |> List.max
  let minY = positions |> List.map snd |> List.min
  let maxY = positions |> List.map snd |> List.max
  [
    tileGrid |> Map.find (minX, minY);
    tileGrid |> Map.find (minX, maxY);
    tileGrid |> Map.find (maxX, maxY);
    tileGrid |> Map.find (maxX, minY)
  ]
  |> List.map (fun tile -> tile.Id)
  |> bigintProductOfInts


let solveB input = ""

let solve = solveDay solveA solveB

let testInput =
  "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."