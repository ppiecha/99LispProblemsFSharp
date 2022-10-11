(*
P40 (**) Goldbach's conjecture.
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
Example: 28 = 5 + 23.
It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
It has been numerically confirmed up to very large numbers (much larger than we can go with our Lisp system).
Write a function to find the two prime numbers that sum up to a given even integer.
Example:
* (goldbach 28)
(5 23)
*)

let isPrime n =
    let sqrtN = sqrt (float n) |> int
    match n with
    | 1 -> false
    | 2 -> true
    | n -> seq {2 .. sqrtN} |> Seq.exists (fun x -> n % x = 0) |> not 

let primesInRange n m =
    let start = if n % 2 = 0 then n + 1 else n
    let range = [start .. 2 .. m]
    let range = if start <= 2 then 2 :: range else range
    range |> List.filter isPrime

let goldbach n =
    if n <= 2 then failwith $"Number must be greater than 2"
    let primes = primesInRange 2 (n - 1)
    primes |> List.find (fun x -> List.exists (fun y -> (n - x) = y) primes) |> fun x -> x, n - x
     
goldbach 28
goldbach 30
goldbach 2