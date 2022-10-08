open Batteries
open Alcotest

open Jcat.Machine
open Instructions

let pass x = match infer x with
  | () -> true
  | exception _ -> false

let tests = [
  (* true, expr [quote @@ expr [int 0]; call];  *)

  true, expr [int 0; int 1; plus]; 
  false, expr [int 0; bool true; plus]; 
  true, expr [int 0; quote @@ expr [int 1; plus]; call]; 
  false, expr [bool false; quote @@ expr [int 1; plus]; call]; 
  true, expr [plus]; (* edgecase of this system: main can be whatever; no empty stacks. *)
  true, expr []; 
  true, expr [int 0; int 1; quote @@ expr []; call; plus]; 
  false, expr [int 0; int 1; quote @@ expr []; plus]; 
  false, expr [int 0; bool false; quote @@ expr []; call; plus]; 

  (* true, expr [quote @@ expr [int 0; int 1; plus]; call];  *)
  (* true, expr [int 0; quote @@ expr [int 1; plus]; call; plus];  *)
]

let same = check Alcotest.bool "same bool"

let () = [
  "main", 
  tests |> List.map begin fun (c, e) -> 
    (fun () -> same c (pass e))
    |> test_case (string_of_int (unique ())) `Quick
  end
] |> run "Tests"
