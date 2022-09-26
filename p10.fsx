(*
P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:
* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
*)

let pack list =
    let collect x = function
        | (h :: t1) :: t2 when x = h -> (x :: h :: t1) :: t2
        | lst -> [x] :: lst
    List.foldBack collect list []
    
let encode list = list |> pack |> List.map (fun x -> [string (List.length x); string (List.head x)])

encode ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']

let encode2 list = list |> pack |> List.map (fun x -> (List.length x, List.head x))

encode2 ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']


