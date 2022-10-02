(*
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list.
Example:
* (lotto-select 6 49)
(23 1 17 33 21 37)

Hint: Combine the solutions of problems P22 and P23.
*)

let range (start : int) (stop : int) =
    let rec range' start' step' stop' =
        if start' = stop' then [stop']
        else start' :: range' (start' + step') step' stop'
    range' start (sign (stop - start)) stop
    
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
        
let lottoSelect N M = randomSelect (range 1 M) N

lottoSelect 6 49