(*
P20 (*) Remove the K'th element from a list.
Example:
* (remove-at '(a b c d) 2)
(A C D)
*)

let removeAt list index =
    let rec loop list counter =
        match list with
        | [] -> []
        | h :: t -> if counter = index then loop t (counter + 1) else h :: loop t (counter + 1)
    loop list 1
    
removeAt ['a'; 'b'; 'c'; 'd'] 2    
    