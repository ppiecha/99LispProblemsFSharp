(*
P23 (**) Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
(E D A)
Hint: Use the built-in random number generator and the result of problem P20.
*)

//rand new System.Random()

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

let randomSelect list count = randomRemove list ((List.length list) - count)

randomSelect ['a';'b';'c';'d';'e';'f';'g';'h'] 3