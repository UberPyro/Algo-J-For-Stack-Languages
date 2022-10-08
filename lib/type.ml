open Batteries
open Uref

exception Type_error of string

let pp_uref f z y = f z (uget y)

(* The type language *)
type var_ref = var_type uref
and var_type = 
  | Int
  | Bool
  | Var of int     (* initially fresh *)
  | Quote of fn
and seq_ref = seq_type uref
and seq_type = 
  | Stack of seq_ref * var_ref  (* stacks have a value on top, or be empty *)
  | Bottom of int  (* initially fresh *)
and fn = seq_ref * seq_ref  (* functions are between stacks *)
  [@@deriving show]

let rec unify_seq r = 
  unite ~sel:begin curry @@ function
    | Bottom _ as s, Bottom _ -> s
    | Stack _ as s, Bottom _ | Bottom _, (Stack _ as s) -> s  (* take the longer stack *)
    | Stack (a, t) as s, Stack (b, u) -> 
      unify_var t u;  (* unify the top elements *)
      unify_seq a b;  (* unify the rest *)
      s
  end r

and unify_var r = 
  unite ~sel:begin curry @@ function
    | Quote (i0, o0) as q, Quote (i1, o1) -> 
      unify_seq i0 i1; (* unified functions have same inputs *)
      unify_seq o0 o1; (* and outputs *)
      q
    | v, Var _ | Var _, v -> v  (* unified var types are the same *)
    | v, u when v = u -> v  (* unifying something with itself is itself *)
    | v, u -> 
      raise @@ Type_error begin
        Printf.sprintf
          "type error: [%s] not compatible with [%s]"
          (show_var_type v)
          (show_var_type u)
      end
  end r
