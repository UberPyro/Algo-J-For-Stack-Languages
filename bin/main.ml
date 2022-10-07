open Jcat.Mach
open Expr
 
let prog = [
  Num 4; Quote [Num 5; Plus]; Call
]

let () = check prog 
