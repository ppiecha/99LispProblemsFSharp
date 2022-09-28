(*
P15 (**) Replicate the elements of a list a given number of times.
Example:
* (repli '(a b c) 3)
(A A A B B B C C C)
*)

let replicate list count =
    List.collect (List.replicate count) list
    
replicate ["a"; "b"; "c"] 3    