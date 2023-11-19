// For more information see https://aka.ms/fsharp-console-apps
open System
open Evaluator
open Parser

[<EntryPoint>]

let main argv = 

    let s = "Usage: <season>: <n> <treetype> <size> size, ...
        \n Note that <size> is 'random' or a float (e.g. '1.0'), and must be from 0.25 - 2"

    (*Fail if not only one arguement is inputted*)
    if Array.length argv <> 1 then failwith s
    
    match parse argv[0] with
    | Some ast -> 
        let svg = (eval ast)
        IO.File.WriteAllText("forest.svg", svg)
        printfn "%s" svg
        0
    | None ->
        printfn "%s" s
        1

