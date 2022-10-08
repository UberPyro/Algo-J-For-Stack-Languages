open Batteries
open Uref

open Type

type 'a node = {data : 'a; ty : Type.fn}

(* expression language *)
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

(* V => 0 => 0 V *)
let push_var v s = 
  uref @@ Stack (s, v)

(* Lit => 0 -- 0 Lit *)
let fresh_lit lit = 
  let s = fresh_stack () in
  s, push_var lit s

let ascribe data ty = {data; ty}

(* provides constructors with correct types *)
module Instructions = struct
  (* 0 -- 0 int *)
  let int i = ascribe (Int i) (fresh_lit (uref Type.Int))
  let bool b = ascribe (Bool b) (fresh_lit (uref Type.Bool))
  (* 0 int int -- 0 int *)
  let plus = ascribe Plus @@
    let s = fresh_stack () in
    s |> push_var (uref Type.Int) |> push_var (uref Type.Int), 
    s |> push_var (uref Type.Int)
  (* 0 [0 -- 1] -- 1 *)
  let call = ascribe Call @@
    let s0, s1 = fresh_stack (), fresh_stack () in
    push_var (uref @@ Type.Quote (s0, s1)) s0, s1
  (* (2 -- 3) => 0 -- 1 [2 -- 3] *)
  let quote fn = ascribe (Quote fn) (fresh_lit (uref @@ Type.Quote fn.ty))
  (* (fn) => fn *)
  let expr e = ascribe e (fresh_stack (), fresh_stack ())
end

(* some expression `e` has some input type stack `io` and output `o0`. 
   it is also composed of concatenated subexpressions with their own input and output types. 
   we must: 
   - unify `i0` with the input of the leftmost subexpression
   - unify `o0` with the output of the rightmost subexpression
   - unify the types of all neighboring subexpressions, so that o(n) = i(n+1) *)
let rec infer {data; ty=i0, o0} = match data with
  | [] -> unify_seq i0 o0  (* identity function must have same input and output *)
  | {ty=i, _; _} :: _ -> 
    unify_seq i0 i;  (* i0 is leftmost input *)
    let rec cat = function
      | {ty=_, o1; _} as h :: ({ty=i2, _; _} :: _ as t) -> 
      infer_word h;  (* types of subexpressions are resolved *)
      unify_seq o1 i2; (* neighboring types are unified *)
      cat t
    | [{ty=_, o; _} as x] -> 
      infer_word x; 
      unify_seq o0 o  (* o0 is rightmost input *)
    | [] -> failwith "Unreachable" in
  cat data

and infer_word {data; _} = match data with
  | Int _ | Bool _ | Plus | Call -> ()
  | Quote fn -> infer fn
