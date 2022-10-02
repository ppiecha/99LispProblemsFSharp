(*
P19 (**) Rotate a list N places to the left.
Examples:
* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)

Hint: Use the predefined functions length and append, as well as the result of problem P17.

*)

let split list (atIndex : int) =
    let rec take list n =
        match list with
        | [] -> []
        | h :: t -> if n >= 1 then h :: take t (n - 1) else take t 0
    let rec drop list n =
        match list with
        | [] -> []
        | h :: t -> if n >= 1 then drop t (n - 1) else h :: drop t 0
    take list atIndex, drop list atIndex

let rotate list switch =
    let abs_switch = if switch > 0 then switch else (List.length list) + switch
    let left, right = split list abs_switch
    right @ left
    
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] -2    