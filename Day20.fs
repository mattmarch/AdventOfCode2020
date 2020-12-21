module Day20

open Common

type Pixel = On | Off

type Side = Top | Right | Bottom | Left

type Edge = Side * Pixel list

type Tile = {
  Id: int
  Pixels: Pixel list list
  Edges: Edge list
}

type MatchingEdge = {
  TileId: int
  MatchingSide: Side
  TileIsFlipped: bool
}

type GridPosition = {
  Position: int * int
  QuarterTurnsCW: int
  Flipped: bool
}

let input = readAll "Day20.txt";

let parsePixel pixelChar =
  match pixelChar with
  | '#' -> On
  | '.' -> Off
  | invalid -> failwithf "Invalid Pixel %c" invalid

let parseTile tileString =
  let tileLines = tileString |> splitBy '\n'
  let id =
    match List.head tileLines with
    | ParseRegex @"Tile (\d+):" [Integer id] -> id
    | invalid -> failwithf "Tile missing id line, instead was %s" invalid
  let pixels =
    tileLines
    |> List.tail
    |> List.map (Seq.map parsePixel >> Seq.toList)
  let topEdge = Top, List.head pixels
  let bottomEdge = Bottom, pixels |> List.last |> List.rev
  let leftEdge = Left, pixels |> List.map List.head
  let rightEdge = Right, pixels |> List.map List.last |> List.rev
  {
    Id = id
    Pixels = pixels
    Edges = [topEdge; rightEdge; bottomEdge; leftEdge]
  }

let parseInput input =
  input
  |> splitByString "\n\n"
  |> List.map parseTile

let findMatchingTileForEdge tileList edgePattern =
  let reversedEdge = List.rev edgePattern
  tileList
  |> List.tryPick (fun tile ->
      tile.Edges
      |> List.tryPick (fun (side, tileEdge) ->
          if tileEdge = edgePattern then
            Some ({TileId = tile.Id; MatchingSide = side; TileIsFlipped = false})
          elif tileEdge = reversedEdge then
            Some ({TileId = tile.Id; MatchingSide = side; TileIsFlipped = true})
          else
            None
          )
  )

let findNeighbourTiles tileList tile = 
  let otherTiles = 
    tileList
      |> List.filter ((<>) tile)
  tile.Edges
  |> List.choose (fun (side, pattern) ->
      match findMatchingTileForEdge otherTiles pattern with
      | Some matchingTile -> Some (side, matchingTile)
      | None -> None
  )

let findTileByTileId tileList tileId =
  tileList |> List.find (fun tile -> tile.Id = tileId)

let getRotation rootTileRotation rootTileSide neighbourTileSide =
  (rootTileRotation +
    match rootTileSide, neighbourTileSide with
    | Top, Bottom | Right, Left | Bottom, Top | Left, Right ->
      0
    | Top, Right | Right, Bottom | Bottom, Left | Left, Top ->
      1
    | Top, Top | Right, Right | Bottom, Bottom | Left, Left ->
      2
    | Top, Left | Right, Top | Bottom, Right | Left, Bottom ->
      3
  ) % 4

let rotatePosition quarterTurnsCW (posX, posY) =
  match quarterTurnsCW with
  | 0 -> posX, posY
  | 1 -> posY, -posX
  | 2 -> -posX, -posY
  | 3 -> -posY, posX
  | invalid -> failwithf "Expected quarterTurnsCW to be between 0 and 3, was %i" invalid

let addPositions (x1, y1) (x2, y2) =
  x1 + x2, y1 + y2

let getNeighbourGridPosition rootTilePosition rootTileSide neighbourMatchingEdge =
  let flipped = rootTilePosition.Flipped <> neighbourMatchingEdge.TileIsFlipped
  let rotation = getRotation rootTilePosition.QuarterTurnsCW rootTileSide neighbourMatchingEdge.MatchingSide
  let relativePositionDelta =
    match rootTileSide with
    | Top -> 0, 1
    | Right -> 1, 0
    | Bottom -> 0, -1
    | Left -> -1, 0
  let position = 
    relativePositionDelta 
    |> rotatePosition rootTilePosition.QuarterTurnsCW
    |> addPositions rootTilePosition.Position
  {
    Position = position
    QuarterTurnsCW = rotation
    Flipped = flipped
  }

let buildGrid tileList =
  let firstTile = List.head tileList
  let initialPosition = {
    Position = (0, 0)
    QuarterTurnsCW = 0
    Flipped = false
  }
  let initialGrid = 
    Map.ofList [(firstTile, initialPosition)]
  let initialPlacedTiles = [firstTile.Id]
  let rec placeNeighboursRecursively (grid: Map<Tile, GridPosition>) placedTiles tile tilePosition =
    findNeighbourTiles tileList tile
    |> List.filter (fun (_, matchedEdge) -> not (List.contains matchedEdge.TileId placedTiles))
    |> List.map (fun (side, matchedEdge) -> 
        (
          findTileByTileId tileList matchedEdge.TileId,
          getNeighbourGridPosition tilePosition side matchedEdge
        )
    )
    |> List.fold (fun grid (tile, position) -> placeNeighboursRecursively grid (tile.Id :: placedTiles) tile position) grid
  placeNeighboursRecursively initialGrid initialPlacedTiles firstTile initialPosition

let solveA input = 
  let tileList = parseInput input
  tileList
  |> buildGrid

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