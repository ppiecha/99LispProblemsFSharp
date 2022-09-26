(*
P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:
* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
*)

let compress list =
    let rec loop acc = function
        | [] -> acc
        | h :: t -> if not (List.isEmpty acc) && List.last acc = h then loop acc t else loop (acc @ [h]) t
    loop [] list
    
let compress2 = function
    | [] -> []
    | h :: t -> List.fold (fun acc x -> if (List.head acc) = x then acc else x :: acc) [h] t |> List.rev
    
let compress3 list = List.foldBack (fun x acc -> if List.isEmpty acc then [x] elif List.head acc = x then acc else x :: acc) list []
    
    
compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
compress2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
compress3 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]        