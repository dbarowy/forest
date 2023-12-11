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

let evalKind (kind: Tree, season: Season) =
    printf("here")
    match season with
    |Fall ->
        printf("here2")
        match kind with
        | Maple -> "fallmaple.svg"
        | Oak -> "falloak.svg"
        | Birch -> "fallbirch.svg"
        | Pine -> "fallpine.svg"
    |Spring ->
        match kind with
        | Maple -> "springmaple.svg"
        | Oak -> "springoak.svg"
        | Birch -> "springbirch.svg"
        | Pine -> "springpine.svg"

//type Grove = {num: int; kind: Tree; size: float}
let evalGrove (grove: Grove, season: Season) : string =
    let treeG =  (IO.File.ReadAllText (evalKind (grove.kind, season))) + "\n</g>\n"

    let treeList = [for i in 0..grove.num - 1 do ((transform grove.size) + treeG) ]
    treeList |> String.Concat
        

let rec evalForest (forest: Forest, season: Season) : string =
    match forest with
    | [] -> ""
    | x::xs -> (evalGrove (x, season)) + (evalForest (xs, season))

let rec evalLandscape (landscape: Landscape) : string =
    printf("here")
    (evalSeason landscape.season) + (evalForest (landscape.forest, landscape.season))

let eval (landscape: Landscape) : string =
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\"" +
    " height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:bx=\"https://boxy-svg.com\">\n" +
    (evalLandscape landscape)
    + "</svg>\n"



