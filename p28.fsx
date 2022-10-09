(*
P28 (**) Sorting a list of lists according to length of sublists
a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Example:
* (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

Example:
* (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
((I J K L) (O) (A B C) (F G H) (D E) (D E) (M N))
*)

let divide list =
    match list with
    | [] -> [], []
    | [x] -> [x], []
    | list -> let index = (List.length list) / 2 in list[.. (index - 1)], list[index ..]
     
divide [1; 2; 3]     
divide [1]

let rec merge list1 list2 key =
    match list1, list2 with
    | [], list2 -> list2
    | list1, [] -> list1
    | (h1 :: t1), (h2 :: t2) -> if key h1 h2 then
                                    h1 :: merge t1 (h2 :: t2) key
                                else
                                    h2 :: merge (h1 :: t1) t2 key                               
    
merge [5] [2; 3] (<)
merge [5; 3; 2; 1] [4; 1] (>)

let rec sortMerge (list : List<'a>) (key: ('a -> 'a -> bool)) =
    match list with
    | [] -> []
    | [x] -> [x]
    | list -> let list1, list2 = divide list in merge (sortMerge list1 key) (sortMerge list2 key) key
    
sortMerge [3; 2] (<)
sortMerge [2; 3; 1] (<)
sortMerge [2; 3; 1; 2; 5; 2] (<)
sortMerge [3; 2; 3; 2; 4; 2; 1] (<)
sortMerge ['b'; 'a'; 'c'] (<)

let lsort list = sortMerge list (fun l1 l2 -> List.length l1 < List.length l2)

lsort [['a'; 'b'; 'c']; ['d'; 'e']; ['f'; 'g'; 'h']; ['d'; 'e']; ['i'; 'j'; 'k'; 'l']; ['m'; 'n']; ['o']]

let lsort' list = list |> List.sortBy List.length

lsort' [['a'; 'b'; 'c']; ['d'; 'e']; ['f'; 'g'; 'h']; ['d'; 'e']; ['i'; 'j'; 'k'; 'l']; ['m'; 'n']; ['o']]

let lfsort list =
    list
    |> List.groupBy (List.length)
    |> List.sortBy (snd >> List.length)
    |> List.collect (snd)
    
lfsort [['a'; 'b'; 'c']; ['d'; 'e']; ['f'; 'g'; 'h']; ['d'; 'e']; ['i'; 'j'; 'k'; 'l']; ['m'; 'n']; ['o']]    