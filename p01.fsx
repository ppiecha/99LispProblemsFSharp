(*
P01 Find the last box of a list.
Example:
* (my-last '(a b c d))
(D)
*)

let rec myLast list =
    match list with
    | [] -> []
    | head :: tail ->
        if List.isEmpty tail then
            [ head ]
        else
            myLast tail

myLast [ 1; 2; 3; 4 ]
