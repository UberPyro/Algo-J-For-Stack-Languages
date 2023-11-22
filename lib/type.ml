open Batteries
open Uref
open Printf

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
  unite ~sel:begin fun s t -> match s, t with
    | Bottom _, Bottom _ -> s
    | Stack _ as x, Bottom i | Bottom i, (Stack _ as x) -> 
      occurs_seq i x; (* ensure one stack is not inside the other *)
      x               (* take the longer stack *)
    | Stack (a, t), Stack (b, u) -> 
      unify_var t u;  (* unify the top elements *)
      unify_seq a b;  (* unify the rest *)
      s
  end r

and occurs_seq i = function
  | Bottom j when i = j -> 
    raise @@ Type_error begin
      sprintf  (* Can never unify a seqvar with itself plus something *)
        "type error: [%d] not compatable with sequence that contains it" i
    end
  | Bottom _ -> ()  (* distinct bottom is safe *)
  | Stack (a, t) -> 
    occurs_var i (uget t);  (* check quotes for more stacks *)
    occurs_seq i (uget a)   (* check rest of stack *)

and unify_var r = 
  unite ~sel:begin curry @@ function
    | Quote (i0, o0) as q, Quote (i1, o1) -> 
      unify_seq i0 i1; (* unified functions have same inputs *)
      unify_seq o0 o1; (* and outputs *)
      q
    | v, u when v = u -> v  (* unifying something with itself is itself *)
    | v, Var k | Var k, v -> 
      occurs_var k v;  (* ensure variable not in type *)
      v                (* unified var types are the same *)
    | v, u -> 
      raise @@ Type_error begin
        sprintf
          "type error: [%s] not compatible with [%s]"
          (show_var_type v)
          (show_var_type u)
      end
  end r

and occurs_var k = function
  | Var j when k = j -> 
    raise @@ Type_error begin
      sprintf  (* Can never unify a var with itself inside something *)
        "type error: [%d] not compatable with type that contains it" k
    end
  | Var _ | Int | Bool -> ()  (* constants and distinct vars are safe *)
  | Quote (i, o) -> 
    occurs_seq k (uget i);  (* check both stacks for more quotes *)
    occurs_seq k (uget o)   (* note that var and seqvar ids are disjoint *)
