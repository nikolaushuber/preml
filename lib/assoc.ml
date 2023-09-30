open K_normal 

(* Inspired by MinCaml compiler *)

let rec trans_expr = function 
  | If (c, t, e) -> If (c, trans_expr t, trans_expr e) 
  | Let (x, t, e1, e2) -> 
    let rec insert = function 
      | Let (x', t', e3, e4) -> Let (x', t', e3, insert e4) 
      | e -> Let (x, t, e, trans_expr e2) 
    in
    insert (trans_expr e1) 
  | e -> e 
  
let trans_func f = 
  let name, (args, ty, e) = f in 
  name, (args, ty, trans_expr e) 

let trans_def = function 
  | Func f -> Func (trans_func f) 
  | RecFunc fs -> RecFunc (List.map trans_func fs) 
  | Expr (name, ty, e) -> Expr (name, ty, trans_expr e) 

let f = List.map trans_def 

