(*
    P04 Find the number of elements of a list.
*)

let count list =
    let rec loop list acc =
        match list with
        | [] -> acc
        | head :: tail -> loop tail (acc + 1)

    loop list 0

printf $"count {count [ 1; 2; 3; 4; 5 ]}\n"

if count [ 1; 2; 3; 4; 5 ] <> 5 then
    failwith "test failed"
