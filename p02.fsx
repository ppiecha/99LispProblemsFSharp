(*
P02 Find the last but one box of a list.
Example:
* (my-but-last '(a b c d))
(C D)
*)

let rec myButLast list =
    match list with
    | [] -> []
    | [x] -> [x]
    | [x; y] -> [x; y] 
    | _ :: t -> myButLast t
    
myButLast ([] : int list) 
myButLast [1]
myButLast [1; 2]
myButLast [1; 2; 3]
myButLast [1; 2; 3; 4]    
    
let kLast k list =
    let rec loop list k index result =
        match list with
        | [] -> result
        | head :: tail when index >= k -> loop (tail) k (index + 1) (result @ [ head ])
        | _ :: tail -> loop (tail) k (index + 1) result

    loop list k 0 []

kLast 2 [ 1; 2; 3; 4; 5; 6 ]

if kLast 2 [ 1; 2; 3; 4; 5; 6 ] <> [ 3; 4; 5; 6 ] then
    failwith "Error"

let kLast' (k : int) list =
    let rec loop counter list =
        match counter, list with
        | _, [] -> []
        | (1 | 2), t -> t
        | _, h :: t -> loop (counter - 1) t
    loop (List.length list) list
    
kLast' 2 ([] : int list) 
kLast' 2 [1]
kLast' 2 [1; 2]
kLast' 2 [1; 2; 3]
kLast' 2 [1; 2; 3; 4]

let myButLast' list = kLast' 2 list 