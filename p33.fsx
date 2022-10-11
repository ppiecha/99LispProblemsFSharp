(*
P33 (*) Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.
Example:
* (coprime 35 64)
T
*)

let rec gcd n m =
    match n, m with
    | n, m when n = m -> n
    | n, m when n > m -> gcd (n - m) m
    | _ -> gcd (m - n) n
    
let coprime n m = (gcd n m) = 1

coprime 35 64