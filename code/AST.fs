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

let breadth = 2.0
let shift = 0.25

let CANVAS_SZ = 2000

let FALL_RGB = "#FF7878"
let SPRING_RGB = "#BEFF7B"

let TREE_SVG_SIZE = 400




