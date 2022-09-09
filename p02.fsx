(*
P02 Find the last but one box of a list.
Example:
* (my-but-last '(a b c d))
(C D)
*)

let rec myButLast list =
    match list with
    | [] -> []
    | head1 :: head2 :: tail ->
        if List.isEmpty tail then
            [ head1; head2 ]
        else
            myButLast tail


let kLast k list =
    let rec loop list k index result =
        match list with
        | [] -> result
        | head :: tail when index >= k -> loop (tail) k (index + 1) (result @ [ head ])
        | head :: tail -> loop (tail) k (index + 1) result

    loop list k 0 []

kLast 2 [ 1; 2; 3; 4; 5; 6 ]

if kLast 2 [ 1; 2; 3; 4; 5; 6 ] <> [ 3; 4; 5; 6 ] then
    failwith "Error"
