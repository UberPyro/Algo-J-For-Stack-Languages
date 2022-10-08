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
  | Stack of seq_ref * var_ref
  | Bottom of int  (* initially fresh *)
and fn = seq_ref * seq_ref
  [@@deriving show]

let rec unify_seq r = 
  unite ~sel:begin curry @@ function
    | Bottom _ as s, Bottom _ -> s
    | Stack _ as s, Bottom _ | Bottom _, (Stack _ as s) -> s
    | Stack (a, t) as s, Stack (b, u) -> 
      unify_var t u; 
      unify_seq a b; 
      s
  end r

and unify_var r = 
  unite ~sel:begin curry @@ function
    | Quote (i0, o0) as q, Quote (i1, o1) -> 
      unify_seq i0 i1; 
      unify_seq o0 o1; 
      q
    | v, Var _ | Var _, v -> v
    | v, u when v = u -> v
    | v, u -> 
      raise @@ Type_error begin
        Printf.sprintf
          "type error: [%s] not compatible with [%s]"
          (show_var_type v)
          (show_var_type u)
      end
  end r
