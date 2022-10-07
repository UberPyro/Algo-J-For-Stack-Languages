open Batteries
open Uref

open Ty

module type Wrapper = sig
  type 'a t
end

module Wrap_expr (W : Wrapper) = struct
  type e = w W.t list W.t
  and w =
    | Num of int
    | Bool of bool
    | Plus
    | Call
    | Quote of e
end

module Expr = Wrap_expr(struct type 'a t = 'a end)
module Ty_expr = Wrap_expr(struct type 'a t = 'a * eff end)

let fresh_stack () =
  uref @@ Bottom (gimme ())

let fresh_endo () : eff =
  fresh_stack (), fresh_stack ()

let push_tau lit b =
  uref @@ Stack (uref lit, b)

let fresh_lit lit : eff =
  let b = fresh_stack () in
  b, push_tau lit b

let rec type_expr (e : Expr.e) : Ty_expr.e =
  List.map type_word e, fresh_endo ()

and type_word =
  let open Ty_expr in function
  | Expr.Num n -> Num n, fresh_lit TInt
  | Bool b -> Bool b, fresh_lit TBool
  | Plus ->
    let s0 = push_tau TInt @@ fresh_stack () in
    Plus, (push_tau TInt s0, s0)
  | Call ->
    let s0 = fresh_stack ()
    and s1 = fresh_stack () in
    Call, (push_tau (TFun (s0, s1)) s0, s1)
  | Quote e ->
    let te = type_expr e in
    Quote te, fresh_lit @@ TFun (snd te)

and infer (e0, (i0, o0)) = match e0 with
  | [] -> unify i0 o0
  | (_, (i, _)) :: _ -> 
    unify i0 i; 
    let rec aux = function
      | (_, (_, o1) as h) :: ((_, (i2, _)) :: _ as t) ->
        infer_word h; 
        unify o1 i2; 
        aux t
      | [_, (_, o) as x] -> 
        infer_word x; 
        unify o0 o
      | [] -> failwith "Unreachable" in
    aux e0

and infer_word (w, (_, _)) =
  let open Ty_expr in match w with
  | Num _ | Bool _ | Plus | Call -> ()
  | Quote e -> infer e

let check = infer % type_expr
