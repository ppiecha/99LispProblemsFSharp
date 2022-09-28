(*
P17 (*) Split a list into two parts; the length of the first part is given.
Do not use any predefined functions.

Example:
* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
*)

let split list (atIndex : int) =
    let rec loop list atIndex acc =
        match list with
        | [] -> acc
        | h :: t ->
            if atIndex >= 1 then loop t (atIndex - 1) ([acc.Item(0) @ [h]; []])
            else loop t 0 [acc.Item(0); (acc.Item(1)) @ [h]]
    loop list atIndex [[]; []]

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3  
    
let split2 list (atIndex : int) =
    let rec take list n =
        match list with
        | [] -> []
        | h :: t -> if n >= 1 then h :: take t (n - 1) else take t 0
    let rec drop list n =
        match list with
        | [] -> []
        | h :: t -> if n >= 1 then drop t (n - 1) else h :: drop t 0
    take list atIndex, drop list atIndex

split2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3  