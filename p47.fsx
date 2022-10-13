(*
P47 (*) Truth tables for logical expressions (2).
Modify problem P46 by accepting expressions written in infix notation, with all parenthesis present.
This allows us to write logical expression in a more natural way, as in the example: (A and (A or (not B))).
Example:
* (table 'A 'B '(A and (A or (not B)))).
true true true
true nil true
nil true nil
nil nil nil
*)

let (^&&) a b = not (a && b)

true ^&& true

let (^||) a b = not (a || b)

true ^|| false 

let (^=) a b = a <> b

true ^= true

let (|=>) a b = compare a b |> (<>) 1

true |=> false

let table f =
    [for i in [true; false] do for j in [true; false] do yield i, j]
    |> List.iter (fun (x, y) -> printfn $"{x} {y} {f x y}")
    
table (fun a b -> a && (a || not b))