(*
P25 (*) Generate a random permutation of the elements of a list.
Example:
* (rnd-permu '(a b c d e f))
(B A D C E F)

Hint: Use the solution of problem P23.
*)

let removeAt list index =
    let rec loop list counter =
        match list with
        | [] -> []
        | h :: t ->
            if counter = index then
                loop t (counter + 1)
            else
                h :: loop t (counter + 1)
    loop list 1

let rec randomRemove list count =
    let rand = System.Random()
    if count = 0 then
        list
    else
        let truncated = removeAt list (rand.Next(List.length list) + 1) 
        randomRemove truncated (count - 1)
        
let rec randomRemoveOne list =
    let rand_index = System.Random().Next(List.length list)
    let rand_value = List.item rand_index list
    let truncated = removeAt list (rand_index + 1) 
    rand_value, truncated        

let randPerm list =
    let rec get_single_elem list times =
        let value, truncated = randomRemoveOne list
        if times = 1 then [value] else value :: get_single_elem truncated (times - 1)
    get_single_elem list (List.length list)

randPerm ['a';'b';'c';'d';'e';'f']

let randPerm' list =
    let randList = let r = System.Random() in List.init (List.length list) (fun _ -> r.Next())
    list |> List.zip randList |> List.sortBy fst |> List.map snd  
    
randPerm' ['a';'b';'c';'d';'e';'f']

