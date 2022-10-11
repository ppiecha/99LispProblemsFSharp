(*
P36 (**) Determine the prime factors of a given positive integer (2).
Construct a list containing the prime factors and their multiplicity.
Example:
* (prime-factors-mult 315)
((3 2) (5 1) (7 1))
Hint: The problem is similar to problem P10.
*)
let primeFactorsMult n =
    let rec primeFactors n =
        let sqrtN n = sqrt(float n) |> int
        let isPrime(n : int) =
            seq {2 .. sqrtN n} |> Seq.exists (fun x -> n % x = 0) |> not
        let first_prime_divisor n =
            if n = 2 then
                None
            else
                seq {yield 2; yield! seq {3 .. 2 .. sqrtN n}} |> Seq.tryFind (fun x -> (n % x = 0) && isPrime x)
        match first_prime_divisor n with
        | Some x -> x :: primeFactors (n / x)
        | None -> [n]
    n |> primeFactors |> List.countBy id 
    
primeFactorsMult 315
primeFactorsMult 315434564    