module Parser
open AST
open Combinator
open System

let r = new Random()

(*Pad lets us have unlimited whitespaces between phrases*)
let pad p = pbetween pws0 p pws0 

(*treeType and season matches typed string to the avaliable options*)
let treeType = 
    (pstr "maple" |>> (fun _ -> Maple)) <|>
    (pstr "oak" |>> (fun _ -> Oak)) <|>
    (pstr "birch" |>> (fun _ -> Birch)) <|>
    (pstr "pine" |>> (fun _ -> Pine))

(* Parse the season, returning the proper Season object *)
let season = 
    (pstr "fall" |>> (fun _ -> Fall)) <|>
    (pstr "spring" |>> (fun _ -> Spring))

(*number function for clarity used to determine Number and Size of Trees*)
let num = pmany1 pdigit

(*BEGIN: DETERMINES NUMBER OF TREES*)
let numInt = (num |>> (fun ds -> stringify ds |> int))

    (*range option differentiated by the '-' character*)
let range = pseq (pleft numInt (pchar '-')) numInt (fun (x, y) -> r.Next(x, y + 1))

    (*randomtINT chosen by a preset min and max set in AST.fs*)
let randomInt = pstr "random" |>> (fun _ -> r.Next(nMIN, nMAX))

    (*Outlines three options for the user: specify a range of trees: "8-20 maple", a number: "7 maple", or a random amount: "random maple"*)
let treecount = 
    range <|>
    numInt <|> 
    randomInt
(*END*)


(*DETERMINES SIZE OF TREES*)
let numFloat = pseq (pleft (num) (pchar '.')) (num) (fun (x, y) -> stringify x + "." + stringify y |> float)

    (*range float calculates a random float from the shift value to the breadth value. This function will favor the breath value due capping the random value at breadth*)
let rangeFloat =
    let x = r.NextDouble() * breadth + shift
    if x > breadth then breadth
    else x

    (*range is from shift to breadth which are set in AST.fs*)
let randomFloat = pstr "random" |>> (fun _ -> rangeFloat)
    
    (*Outlines two options for the user: specify a size: "0.5 size" or set random "random size"*)
let size = 
    pleft (pad (numFloat <|> randomFloat)) (pad (pstr "size") <|> (pstr ""))
(*END*)


(* Parses the number, size, and type of a given grove in the format:
   # TreeType (# size), where size is optional *)
let grove = (pseq
    (pseq (pad treecount) (pad treeType) (fun (x, y) -> (x,y))) 
    ((pad size) <|> (pstr "" |>> (fun _ -> 1.0))) 
    (fun (a, b) -> {num = fst a; kind = snd a; size = b}))

(* Parses the full list of groves separated by commas, putting them into a list *)
let forest = (pseq 
    (pmany0 (pad (pleft grove (pchar ',')))) 
    (pad grove) 
    (fun (x, y) -> x @ [y]))

(* Parses the full landscape, i.e. the season and all the groves of the forest. *)
let landscape = (pseq
    (pleft season (pchar ':'))
    (pad forest)
    (fun (x, y)-> {season = x; forest = y}))


(*checks that the parsing ends at the end of file marker*)
let grammar = pleft landscape peof

(* Determine if parsing was successful, and return Landscape if it was *)
let parse (input: string) : Landscape option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None

