(*
P14 (*) Duplicate the elements of a list.
Example:
* (dupli '(a b c c d))
(A A B B C C C C D D)
*)

let duplicate list =
    List.collect (fun x -> [x; x]) list
    
duplicate ['a'; 'b'; 'c'; 'c'; 'd']

    