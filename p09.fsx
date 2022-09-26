(*
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
*)

let pack list =
    let collect x = function
        | (h :: t1) :: t2 when x = h -> (x :: h :: t1) :: t2
        | lst -> [x] :: lst
    List.foldBack collect list []
    
pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']    
    


