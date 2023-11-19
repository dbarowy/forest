module Evaluator

open AST

let evalSeason (season: Season) : string =
    match season with
    | Fall ->
    | Spring ->
    | Red -> "rgb(255,0,0)"
    | Green -> "rgb(0,255,0)"
    | Blue -> "rgb(0,0,255)"
    | Purple -> "rgb(128,8,165)"

let evalLine (line: Line) : string =
    "  <line x1=\"" + (line.c1.x |> string) + "\"" +
    " y1=\"" + (line.c1.y |> string) + "\"" +
    " x2=\"" + (line.c2.x |> string) + "\"" +
    " y2=\"" + (line.c2.y |> string) + "\"" +
    " style=\"stroke:" +
    (evalColor line.color) + ";stroke-width:2\" />\n"

let rec evalLandscape (landscape: Landscape) : string =
    match canvas with
    | [] -> ""
    | l::ls -> (evalLine l) + (evalCanvas ls)

let eval (landscape: Landscape) : string =
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    (evalLandscape landscape)
    + "</svg>\n"



