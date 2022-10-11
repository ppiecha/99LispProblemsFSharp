(*
P41 (**) A list of Goldbach compositions.
Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
Example:
* (goldbach-list 9 20)
10 = 3 + 7
12 = 5 + 7
14 = 3 + 11
16 = 3 + 13
18 = 5 + 13
20 = 3 + 17
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
    
let goldbachList n m =
    [n .. m]
    |> List.filter (fun x -> x > 2 && x % 2 = 0)
    |> List.map (fun x -> x, goldbach x)
    |> List.iter (fun (n, (p, q)) -> printfn $"{n} = {p} + {q}")
    
goldbachList 10 20