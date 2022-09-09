(*
    P03 Find the K'th element of a list.
    The first element in the list is number 1.
    Example:
    * (element-at '(a b c d e) 3)
    C
*)

let elementAt list num =
    let rec loop list num index =
        match list with
        | [] -> failwith "Empty list"
        | list when num > List.length list -> failwith $"Index {num} out of bound {List.length list}"
        | head :: tail when index = num -> head
        | head :: tail -> loop tail num (index + 1)

    loop list num 1

if elementAt [ 1; 2; 3; 4; 5 ] 3 <> 3 then
    failwith "Test failed"
