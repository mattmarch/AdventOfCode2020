module Day07

open Common

type BagContentItem = { BagName: string; Quantity: int }

type BagContents = BagContentItem list

type BagList = Map<string, BagContents>

let input = readLines "Day07.txt"

let parseBagContent (contentString: string): BagContentItem =
    let itemWords = contentString |> splitBy ' '
    { 
        BagName = itemWords.[1..2] |> String.concat " "
        Quantity = itemWords |> List.head |> int 
    }

let parseLine (line: string): string * BagContents =
    let lineWords = line |> splitBy ' '
    let bagName = lineWords |> List.take 2 |> String.concat " "
    let bagContentsString = lineWords |> List.skip 4 |> String.concat " "
    if bagContentsString = "no other bags." then
        (bagName, [])
    else
        (bagName,
         bagContentsString
         |> splitByString ", "
         |> List.map parseBagContent)

let shinyGold = "shiny gold"

let rec containsShinyGold (bagList: BagList) (bagName: string) =
    let bagContents = bagList.[bagName]
    List.exists (fun c -> c.BagName = shinyGold) bagContents
    || List.exists (fun c -> containsShinyGold bagList c.BagName) bagContents

let solveA input =
    let bags = input |> Seq.map parseLine
    let bagList = bags |> Map
    bags
    |> Seq.map (fst >> containsShinyGold bagList)
    |> Seq.filter id
    |> Seq.length

let rec countBags (bagList: BagList) (bagName: string): int =
    let bagContents = bagList.[bagName]
    1 + (bagContents
        |> List.sumBy (fun c -> c.Quantity * (countBags bagList c.BagName)))

let solveB input =
    let bagList = input |> Seq.map parseLine |> Map
    countBags bagList shinyGold - 1

let solve: string seq -> PartToSolve -> Unit = solveDay solveA solveB

let testInput =
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
      "bright white bags contain 1 shiny gold bag."
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
      "faded blue bags contain no other bags."
      "dotted black bags contain no other bags." ]
    |> List.toSeq

let testInputB =
    [ "shiny gold bags contain 2 dark red bags."
      "dark red bags contain 2 dark orange bags."
      "dark orange bags contain 2 dark yellow bags."
      "dark yellow bags contain 2 dark green bags."
      "dark green bags contain 2 dark blue bags."
      "dark blue bags contain 2 dark violet bags."
      "dark violet bags contain no other bags." ] 
      |> List.toSeq
