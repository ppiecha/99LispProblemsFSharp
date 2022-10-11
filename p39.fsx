(*
P39 (*) A list of prime numbers.
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
*)

let isPrime n =
    let sqrtN = sqrt (float n) |> int
    match n with
    | 1 -> false
    | 2 -> true
    | n -> seq {2 .. sqrtN} |> Seq.exists (fun x -> n % x = 0) |> not
    
[1 .. 11] |> List.map (fun x -> x, isPrime x)    

let primesInRange n m =
    let start = if n % 2 = 0 then n + 1 else n
    let range = [start .. 2 .. m]
    let range = if start <= 2 then 2 :: range else range
    range |> List.filter isPrime
    
primesInRange 1 23
primesInRange 3 11
primesInRange 3 3   