(*
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:
* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
*)

open System

type ListElem<'a> =
    | Elem of 'a
    | Comp of (int * 'a)
    
let pack list =
    let collect x = function
        | (h :: t1) :: t2 when x = h -> (x :: h :: t1) :: t2
        | lst -> [x] :: lst
    List.foldBack collect list []
    
let encodeModified list =
    list
    |> pack
    |> List.map (fun x -> if List.length x > 1 then Comp (List.length x, List.head x) else Elem (List.head x))
    
encodeModified ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']        