(*
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)
*)

let slice list (i : int) (k : int) =
    let rec take n list =
        match list with
        | [] -> []
        | h :: t -> if n >= 1 then h :: take (n - 1) t else take 0 t
    let rec drop n list =
        match list with
        | [] -> []
        | h :: t -> if n >= 1 then drop (n - 1) t else h :: drop 0 t
    list |> drop (i - 1) |> take (k - i + 1)

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3 7     
    