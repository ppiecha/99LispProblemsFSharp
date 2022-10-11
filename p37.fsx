(*
P37 (**) Calculate Euler's totient function phi(m) (improved).
See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
phi(m) = (p1 - 1) * p1 ** (m1 - 1) * (p2 - 1) * p2 ** (m2 - 1) * (p3 - 1) * p3 ** (m3 - 1) * ...

Note that a ** b stands for the b'th power of a.
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
    
primeFactorsMult 10

let totientPhiImproved (n : int) =
    primeFactorsMult n |> List.fold (fun state (p, m) -> state * (p - 1) * pown p (m - 1)) 1
    
totientPhiImproved 10    