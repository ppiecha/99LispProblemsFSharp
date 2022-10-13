(*
P46 (**) Truth tables for logical expressions.
Define functions and, or, nand, nor, xor, impl and equ (for logical equivalence) which return the result of the respective operation on boolean values.
A logical expression in two variables can then be written in prefix notation, as in the following example: (and (or A B) (nand A B)).

Write a function table which prints the truth table of a given logical expression in two variables.

Example:
* (table 'A 'B '(and A (or A B))).
true true true
true nil true
nil true nil
nil nil nil
*)

let and' = (&&)

let or'  = (||)

let nand a b = not (and' a b) 

let nor a b = not (or' a b)

let xor a b = a <> b

let impl a b = compare a b |> (<>) 1

let eq = (=)

let table f =
    [for i in [true; false] do for j in [true; false] do yield i, j]
    |> List.iter (fun (x, y) -> printfn $"{x} {y} {f x y}")

let f a b = and' a (or' a b)

table f