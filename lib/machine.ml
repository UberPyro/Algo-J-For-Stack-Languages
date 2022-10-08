open Batteries
open Uref

open Type

type 'a node = {data : 'a; ty : Type.fn}

type expr = _expr node
and _expr = word list
and word = _word node
and _word = 
  | Int of int
  | Bool of bool
  | Plus
  | Call
  | Quote of expr

let fresh_stack () = 
  uref @@ Bottom (unique ())

let push_var v s = 
  uref @@ Stack (s, v)

let fresh_lit lit = 
  let s = fresh_stack () in
  s, push_var lit s

let ascribe data ty = {data; ty}

module Instructions = struct
  let int i = ascribe (Int i) (fresh_lit (uref Type.Int))
  let bool b = ascribe (Bool b) (fresh_lit (uref Type.Bool))
  let plus = ascribe Plus @@
    let s = fresh_stack () in
    s |> push_var (uref Type.Int) |> push_var (uref Type.Int), 
    s |> push_var (uref Type.Int)
  let call = ascribe Call @@
    let s0, s1 = fresh_stack (), fresh_stack () in
    push_var (uref @@ Type.Quote (s0, s1)) s0, s1
  let quote fn = ascribe (Quote fn) (fresh_lit (uref @@ Type.Quote fn.ty))
  let expr e = ascribe e (fresh_stack (), fresh_stack ())
end

let rec infer {data; ty=i0, o0} = match data with
  | [] -> unify_seq i0 o0
  | {ty=i, _; _} :: _ -> 
    unify_seq i0 i; 
    let rec cat = function
      | {ty=_, o1; _} as h :: ({ty=i2, _; _} :: _ as t) -> 
      infer_word h; 
      unify_seq o1 i2; 
      cat t
    | [{ty=_, o; _} as x] -> 
      infer_word x; 
      unify_seq o0 o
    | [] -> failwith "Unreachable" in
  cat data

and infer_word {data; _} = match data with
  | Int _ | Bool _ | Plus | Call -> ()
  | Quote fn -> infer fn
