// For more information see https://aka.ms/fsharp-console-apps
open cs334
open Combinator

[<EntryPoint>]

let main argv = 

    let s = "Usage: dotnet run <polynomial>\n\twhere <polynomial> has the form <c_1>x^<e_1> + ... + <c_n>x^<e_n>,\n\tfor example, \"3x^5 + -5x^2\""

    (*Fail not only one arguement is inputted*)
    if Array.length argv <> 1 then failwith s
    
    match parse argv[0] with
    | Some ast -> printfn "%A" (prettyprint ast)
    | None -> printfn "%s" s
    0 

