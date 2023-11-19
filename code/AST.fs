module AST

type Tree =
| Maple
| Oak
| Birch
| Pine

type Season =
| Spring
| Fall

type Grove = {num: int; kind: Tree; size: float}

type Forest = Grove list

type Landscape = {season: Season; forest: Forest}

let nMIN = 1
let nMAX = 20

let breadth = 5.00
let shift = 0.25




