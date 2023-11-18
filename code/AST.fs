module AST

type Tree =
| Maple
| Oak
| Birch
| Pine

type Season =
| Spring
| Fall

type Grove = {kind: Tree; size: float; num: int}

type Forest = Grove list

type Landscape = {s: Season; f: Forest}

let nMIN = 1
let nMAX = 20

let breadth = 5.00
let shift = 0.25




