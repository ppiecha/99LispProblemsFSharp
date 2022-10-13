(*
    P05 Reverse a list.
*)

let rec reverse list =
    match list with
    | [] -> []
    | head :: tail -> reverse tail @ [ head ]

if reverse [ 1; 2; 3; 4; 5; 6 ]
   <> [ 6; 5; 4; 3; 2; 1 ] then
    failwith "test failed"

(*
    P06 Palindrome
*)

let palindrome list = list = reverse list

