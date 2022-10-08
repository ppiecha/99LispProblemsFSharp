(*
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:
* (combination 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )
*)

let rec combinations n list =
    match list, n with
    | [], _ -> [[]]
    | list, 1 -> [for x in list -> [x]]
    | h :: t, n -> [for x in combinations (n - 1) t do
                        yield h :: x
                    if List.length t > n then
                        yield! combinations n t
                    else
                        yield t]
    
    
combinations 2 ['a'; 'b'; 'c']
combinations 0 ['a'; 'b'; 'c']
combinations 0 ['a'; 'b']
combinations 0 ['a']

let rec tails = function
    | [] -> [[]]
    | _ :: t as list -> list :: tails t

tails ['a'; 'b'; 'c']

let rec combinations' n list =
    match list, n with
    | _, 0 -> [[]]
    | list, n -> [for tail in tails list do
                     match tail with
                     | [] -> ()
                     | h :: t -> for combination in combinations' (n - 1) t do
                                     yield h :: combination]

combinations' 2 ['a'; 'b'; 'c']
