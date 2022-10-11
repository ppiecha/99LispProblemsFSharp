(*
P35 (**) Determine the prime factors of a given positive integer.
Construct a flat list containing the prime factors in ascending order.
Example:
* (prime-factors 315)
(3 3 5 7)
*)

let isPrime(n : int) =
    let sqrtN = sqrt(float n) |> int
    seq {2 .. sqrtN} |> Seq.exists (fun x -> n % x = 0) |> not

let first_prime_divisor n =
    let sqrtN = sqrt(float n) |> int
    if n = 2 then
        None
    else
        seq {yield 2; yield! seq {3 .. 2 .. sqrtN}} |> Seq.tryFind (fun x -> (n % x = 0) && isPrime x)

[2 .. 11] |> List.map (fun x -> x, first_prime_divisor x)  

let rec primeFactors n =
    match first_prime_divisor n with
    | Some x -> x :: primeFactors (n / x)
    | None -> [n]
    
[2 .. 11] |> List.map (fun x -> x, primeFactors x)
primeFactors 315
primeFactors 315434564