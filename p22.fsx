(*
P22 (*) Create a list containing all integers within a given range.
If first argument is smaller than second, produce a list in decreasing order.
Example:
* (range 4 9)
(4 5 6 7 8 9)
*)

let range (start : int) (stop : int) =
    let rec range' start' step' stop' =
        if start' = stop' then [stop']
        else start' :: range' (start' + step') step' stop'
    range' start (sign (stop - start)) stop

range 4 9
range 9 4
            
let range2 (start : int) (stop : int) =
    if start <= stop then [for i in [start .. stop] -> i] else [for i in [start .. -1 .. stop] -> i] 
          
range2 4 9
range2 9 4