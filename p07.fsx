(*
    P07 (**) Flatten a nested list structure.
    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

    Example:
    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Hint: Use the predefined functions list and append.
*)

type NestedList<'a> =
    | List of NestedList<'a> list
    | Elem of 'a

let rec flatten list =
    let rec loop = function
        | [] -> []
        | h :: t ->
            match h with
                | Elem x -> x :: loop t
                | List l -> loop l @ loop t
    loop [list]
    
flatten (Elem 1)

flatten (List [Elem 1; Elem 2])

flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]])