(*
P32 (**) Determine the greatest common divisor of two positive integer numbers.
Use Euclid's algorithm.
Example:
* (gcd 36 63)
9
*)

let rec gcd n m =
    match n, m with
    | n, m when n = m -> n
    | n, m when n > m -> gcd (n - m) m
    | _ -> gcd (m - n) n
    
gcd 36 63    