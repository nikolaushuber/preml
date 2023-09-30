open K_normal 

(* Inspired by MinCaml compiler *)

let find env x = 
  try 
    List.assoc x env 
  with 
    Not_found -> x  

let counter = ref 0 
let make_unique id = 
  incr counter;
  id ^ (string_of_int !counter) 

let rec trans_expr env = function 
  | Unit -> Unit 
  | Int i -> Int i 
  | Bool b -> Bool b 
  | Float f -> Float f 
  | UnOp (op, x) -> UnOp (op, find env x) 
  | BinOp (op, x, y) -> BinOp (op, find env x, find env y) 
  | If (c, t, e) -> If (find env c, trans_expr env t, trans_expr env e) 
  | Var x -> Var (find env x) 
  | Let (x, t, e1, e2) -> 
    let x' = make_unique x in 
    Let (x', t, trans_expr env e1, trans_expr ((x, x') :: env) e2)
  | App (x, xs) -> App (find env x, List.map (find env) xs)

let trans_func f = 
  let name, (args, ty, e) = f in 
  name, (args, ty, trans_expr [] e) 

let trans_def = function 
  | Func f -> Func (trans_func f) 
  | RecFunc fs -> RecFunc (List.map trans_func fs) 
  | Expr (name, ty, e) -> Expr (name, ty, trans_expr [] e) 

let f = List.map trans_def 

