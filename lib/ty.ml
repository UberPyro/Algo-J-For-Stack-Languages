open Batteries
open Uref

let c = ref (-1)
let c_stack = ref (-1)
let gimme () = incr c; Printf.sprintf "a%d" !c
let gimme_stack () = incr c_stack; Printf.sprintf "%d" !c_stack

type tau = 
  | TInt
  | TBool
  | TVar of string
  | TFun of eff
and io = io_c uref
and io_c = 
  | Stack of tau uref * io
  | Bottom of string
and eff = io * io

let str_of_tau_simple = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar s -> Printf.sprintf "var %s" s
  | TFun _ -> "quote"

let rec unify r = 
  unite ~sel:begin fun a0 b0 -> match a0, b0 with
    | Bottom _ as s, Bottom _ -> s
    | Stack _ as s, Bottom _ | Bottom _, (Stack _ as s) -> s
    | Stack (t, a) as s, Stack (u, b) -> 
      unify_tau t u; 
      unify a b; 
      s
  end r

and unify_tau r = 
  unite ~sel:begin fun t0 u0 -> match t0, u0 with
    | TFun (i0, o0) as f, TFun (i1, o1) -> 
      unify i0 i1; 
      unify o0 o1; 
      f
    | x, TVar _ | TVar _, x -> x
    | x, y when x = y -> x
    | x, y -> 
      Printf.sprintf "type error: [%s] not compatible with [%s]"
        (str_of_tau_simple x)
        (str_of_tau_simple y)
      |> failwith
  end r
