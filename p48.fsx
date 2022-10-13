(*
P48 (**) Truth tables for logical expressions (3).
Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table in a way that (table List Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
Example:
* (table '(A B C) '((A and (B or C)) equ ((A and B) or (A and C)))).
true true true true
true true nil true
true nil true true
true nil nil true
nil true true true
nil true nil true
nil nil true true
nil nil nil true
*)

let table f =
    [for i in [true; false] do for j in [true; false] do yield i, j]
    |> List.iter (fun (x, y) -> printfn $"{x} {y} {f x y}")
    
table (fun a b -> a && (a || not b))