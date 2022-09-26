(*
P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
*)

type ListElem<'a> =
    | Elem of 'a
    | Comp of (int * 'a)
    
//let rec decompress (list: 'a ListElem list) = function
//    | [] -> []
//    | h :: t ->
//        match h with
//        | Elem as e -> e :: decompress t
//        | Comp as comp -> let (num, value) = comp in List.init num (fun x -> value) :: decompress t
        
let decompress (list: 'a ListElem list) =
    List.collect (function Elem e -> [e] | Comp (num, value) -> List.init num (fun x -> value)) list
        
decompress [Comp (4, 'a'); Elem 'b'; Comp (2, 'c'); Comp (2, 'a'); Elem 'd'; Comp (4, 'e')]        
       