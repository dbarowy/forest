module cs334
open Combinator
 
(*These are the AST Types: either we consider a term, or the list of the terms*)
type Term = { coeff: int; exp: int }
type Polynomial = Term list

(*BEGIN PARSING*)
let expr, exprImpl = recparser()

(*number function determines if it is a digit, and if there are multiply digits, strings them together
and casts it into an int
Alongside the number function, posneg sees if there is a - sign infront of a digit, and accordingly makes
it positive or negative*)
let number = (pmany1 pdigit |>> (fun ds -> stringify ds |> int))
let posneg = (pright (pchar '-') number |>> (fun x -> -1 * x)) <|> (number)

(*term parses a Term, and finds the numbers in the terms by seeing what is left and right of the 'x^'.
This logic looks like, [3x] [^4] where the 'x' and the '^' are subsequently thrown out, and the 
3, 4 get cast into a Term type*)
let term : Parser<Term> = pseq (pleft posneg (pchar 'x')) (pright (pchar '^') posneg) (fun (x, y) -> {coeff = x ; exp = y })

(*exprImpl is a parser that uses pseq to differentiate having 1 or more terms in the input.
pseq combines pmany0 amount of terms (which could be none) and one term.
The function part of pseq combines the appends the two lists together*)
exprImpl := 
    pseq
        (pmany0 (pleft (term) (pstr " + ") ) )
        (term |>> (fun i -> [i]))
        (fun (x, y) -> List.append x y)

(*checks that the parsing ends at the end of file marker*)
let grammar = pleft expr peof
(*END PARSING*)


(*Begin EVALUATION*)
(*evalTerm checks if the number is coefficient is zero and if the derivitive goes to zero. In both these cases
the term is set to equal zero so that the derivitive does not keep on going past the point it should.
The other case is where the power rule is applied. The term is modified so the derivitive is taken*)
let evalTerm (ast : Term) : Term =
    match ast.coeff, ast.exp with 
    | 0, _ -> { coeff = 0; exp = 1 }
    | _, 0 -> { coeff = 0; exp = 1 }
    | co, ex -> { coeff = co * ex; exp = ex - 1 }
(*evalPoly plugs each term in the polynomial list into evalTerm. It then sorts by exponent power*)
let evalPoly (poly: Polynomial) : Polynomial =
    let evaluated = poly |> List.map(fun x -> evalTerm x)
    evaluated |> List.sortByDescending(fun x -> x.exp)
(*END EVALUTATION*)

(*Checks if parsing is suscessful. Returns nothing if failed*)
let parse (input : string) : Polynomial option =
    match grammar (prepare input) with
    | Success (es, _) -> Some (evalPoly es)
    | Failure (_, _) -> None

(*Pretty print recursively examines each term in the list, and formats in an easy to read way*)
let rec prettyprint (ast : Polynomial) : string = 
    match ast with
    |[] -> ""
    | x::[] -> (sprintf "%dx^%d" x.coeff x.exp)
    | x::xs -> (sprintf "%dx^%d + " x.coeff x.exp) + prettyprint xs



