(*
P31 (**) Determine whether a given integer number is prime.
Example:
* (is-prime 7)
T
*)

let isPrime(n : int) =
    let sqrtN = sqrt(float n) |> int
    seq {2 .. sqrtN} |> Seq.exists (fun x -> n % x = 0) |> not

[2 .. 11] |> List.map (fun x -> x, isPrime x)


