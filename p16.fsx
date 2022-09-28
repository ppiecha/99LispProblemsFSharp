(*
P16 (**) Drop every N'th element from a list.
Example:
* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
*)

let drop list nth =
    let rec loop list counter acc =
        match list with
        | [] -> acc
        | h :: t -> if counter = 1 then loop t nth acc else loop t (counter - 1) (acc @ [h])
    loop list nth []
    
let drop2 list nth =
    let rec loop list counter =
        match list, counter with
        | [], _ -> []
        | _ :: t, 1 -> loop t nth
        | h :: t, _ -> h :: loop t (counter - 1)
    loop list nth
    
drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3

drop2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3  