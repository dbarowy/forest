module Parser
open AST
open Combinator
open System

let r = new Random()

(*Pad lets us have unlimited whitespaces between phrases*)
let pad p = pbetween pws0 p pws0 

let expr, exprImpl = recparser()


(*treeType and season matches typed string to the avaliable options*)
let treeType = 
    (pstr "maple" |>> (fun _ -> Maple)) <|>
    (pstr "oak" |>> (fun _ -> Oak)) <|>
    (pstr "birch" |>> (fun _ -> Birch)) <|>
    (pstr "pine" |>> (fun _ -> Pine))

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
    pleft (numFloat <|> randomFloat) (pstr "size")
(*END*)


(*
let treefit = pseq (pseq (treecount treeType (fun (x, y) -> (x,y))) size (fun (a, b) -> {kind = a.first; num = a.second; size = b}))

let grove = pseq treecount treeType size
let term : Parser<Term> = pseq (pleft posneg (pchar 'x')) (pright (pchar '^') posneg) (fun (x, y) -> {coeff = x ; exp = y })


exprImpl := 
    pseq
        (pmany0 (pleft (term) (pstr " + ") ) )
        (term |>> (fun i -> [i]))
        (fun (x, y) -> List.append x y)

*)

(*checks that the parsing ends at the end of file marker*)
let grammar = pleft expr peof

