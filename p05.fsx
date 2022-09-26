(*
    P05 Reverse a list.
*)

let rec reverse list =
    match list with
    | [] -> []
    | head :: tail -> reverse tail @ [ head ]

printfn $"result {reverse [ 1; 2; 3 ]}\n"

if reverse [ 1; 2; 3; 4; 5; 6 ]
   <> [ 6; 5; 4; 3; 2; 1 ] then
    failwith "test failed"

(*
    P06 Palindrome
*)

let palindrome list = list = reverse list

printfn $"this is example of embedded {1 + 2} string "


printfn $"""this is example of "embedded" {"a" + "b"} string """

printfn $"""palindrome? {palindrome [ "a"; "b"; "a" ]}"""
