(*
    P07 (**) Flatten a nested list structure.
    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

    Example:
    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Hint: Use the predefined functions list and append.
*)

type 'a NestedList = 
    | List of 'a NestedList list 
    | Elem of 'a

type NestedList2<'a> =
    | List of NestedList2<'a>
    | Elem of 'a

