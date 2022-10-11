(*
P34 (**) Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

* (totient-phi 10)
4
*)

let rec gcd n m =
    match n, m with
    | n, m when n = m -> n
    | n, m when n > m -> gcd (n - m) m
    | _ -> gcd (m - n) n
    
let coprime n m = (gcd n m) = 1

let totientPhi n =
    seq {1 .. (n - 1)} |> Seq.filter (coprime n) |> Seq.length
    
totientPhi 10    