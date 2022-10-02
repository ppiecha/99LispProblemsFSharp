(*
P21 (*) Insert an element at a given position into a list.
Example:
* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
*)

let insertAt elem list index =
    let rec loop list counter =
        match list with
        | [] -> []
        | h :: t ->
            if counter = index then elem :: h :: loop t (counter + 1)
            else h :: loop t (counter + 1)
    loop list 1
    
insertAt 'x' ['a'; 'b'; 'c'; 'd'] 2    