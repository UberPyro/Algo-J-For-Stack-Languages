open Batteries
open Alcotest

open Jcat.Machine
open Instructions

let pass x = match infer x with
  | () -> true
  | exception _ -> false

let tests = [
  true, expr [int 0; int 1; plus]; 
  false, expr [int 0; bool true; plus]; 
  true, expr [int 0; quote @@ expr [int 1; plus]; call]; 
  false, expr [bool false; quote @@ expr [int 1; plus]; call]
]

let same = check Alcotest.bool "same bool"

let () = [
  "main", 
  tests |> List.map begin fun (c, e) -> 
    (fun () -> same c (pass e))
    |> test_case (string_of_int (unique ())) `Quick
  end
] |> run "Tests"
