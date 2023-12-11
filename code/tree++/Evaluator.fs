module Evaluator

open AST
open System

let r = new Random()

(* Transforms an arbitrary SVG tree given a certain size, returning a wrapper for the the resized,
   randomly positioned tree *)
let transform (size: float) =
    let scaledSVGSize = (Math.Round((TREE_SVG_SIZE |> double) * size) |> int)
    let x = r.Next (CANVAS_SZ - (Math.Round((TREE_SVG_SIZE |> double) * size) |> int))
    let y = r.Next (CANVAS_SZ - (Math.Round((TREE_SVG_SIZE |> double) * size) |> int))
    "<g transform=\"translate(" + (x |> string) + ", " + (y |> string) + ") scale(" + (size |> string) + ")\">\n"

(* Evaluates the season within the forest, changing the background of the image accordingly *)
let evalSeason (season: Season) : string =
    match season with
    | Fall -> "<rect fill=\"" + FALL_RGB + "\" width=\"2000\" height=\"2000\"  />"
    | Spring -> "<rect fill=\"" + SPRING_RGB + "\" width=\"2000\" height=\"2000\"  />"

(* Evaluates the TreeKind and obtains the matching svg file with tree graphic data for that kind 
   Also determines if the fall or spring file should be used *)
let evalKind (kind: Tree, season: Season) =
    match season with
    |Fall ->
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

(* Evaluates a grove of trees of one tree type, returning a string of all SVG graphic info for those trees *)
let evalGrove (grove: Grove, season: Season) : string =
    let treeG =  (IO.File.ReadAllText (evalKind (grove.kind, season))) + "\n</g>\n"

    let treeList = [for i in 0..grove.num - 1 do ((transform grove.size) + treeG) ]
    treeList |> String.Concat
        
(* Evaluates a forest of several groves, recursively determining the SVG data for every grove according to
   their size & number *)
let rec evalForest (forest: Forest, season: Season) : string =
    match forest with
    | [] -> ""
    | x::xs -> (evalGrove (x, season)) + (evalForest (xs, season))

(* Evaluates the landscape, including the season of the forest and all of its groves, returning the
   complete SVG data for the forest *)
let rec evalLandscape (landscape: Landscape) : string =
    (evalSeason landscape.season) + (evalForest (landscape.forest, landscape.season))

(* Does final evaluation on the full forest's SVG data, adding necessary tags to make the image renderable
   and ensure it is a certain canvas size. Returns the final SVG data, which can be put into an SVG file
   to make an image *)
let eval (landscape: Landscape) : string =
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\"" +
    " height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:bx=\"https://boxy-svg.com\">\n" +
    (evalLandscape landscape)
    + "</svg>\n"
