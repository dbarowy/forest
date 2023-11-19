module Evaluator

open AST
open System

let r = new Random()

let transform (size: float) =
    let scaledSVGSize = (Math.Round((TREE_SVG_SIZE |> double) * size) |> int)
    let x = r.Next (CANVAS_SZ - (Math.Round((TREE_SVG_SIZE |> double) * size) |> int))
    let y = r.Next (CANVAS_SZ - (Math.Round((TREE_SVG_SIZE |> double) * size) |> int))
    "<g transform=\"translate(" + (x |> string) + ", " + (y |> string) + ") scale(" + (size |> string) + ")\">\n"

let evalSeason (season: Season) : string =
    match season with
    | Fall -> "<rect fill=\"" + FALL_RGB + "\" width=\"2000\" height=\"2000\"  />"
    | Spring -> "<rect fill=\"" + SPRING_RGB + "\" width=\"2000\" height=\"2000\"  />"

let evalKind (kind: Tree) =
    match kind with
    | Maple -> "springmaple.svg"
    | Oak -> "springoak.svg"
    | Birch -> "springbirch.svg"
    | Pine -> "springpine.svg"

//type Grove = {num: int; kind: Tree; size: float}
let evalGrove (grove: Grove) : string =
    let treeG =  (IO.File.ReadAllText (evalKind grove.kind)) + "\n</g>\n"

    let treeList = [for i in 0..grove.num do ((transform grove.size) + treeG) ]
    treeList |> String.Concat
        

let rec evalForest (forest: Forest) : string =
    match forest with
    | [] -> ""
    | x::xs -> (evalGrove x) + (evalForest xs)

let rec evalLandscape (landscape: Landscape) : string =
    (evalSeason landscape.season) + (evalForest landscape.forest)

let eval (landscape: Landscape) : string =
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\"" +
    " height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:bx=\"https://boxy-svg.com\">\n" +
    (evalLandscape landscape)
    + "</svg>\n"



