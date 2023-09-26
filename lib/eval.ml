open Ast 

exception TypeError 
exception UnknownName of string 

type value = 
  | Int of int 
  | Bool of bool 
  | Float of float 
  | Unit 

type closure = expr * env 
and env = Env of {
  values : (string * value) list; 
  funcs : (string * closure) list; 
}

let empty_env = Env { values = []; funcs = [] }

let rec eval_expr env = function 
  | Ast.Unit _ -> Unit 
  | Float { value; _ } -> Float value 
  | Bool { value; _ } -> Bool value 
  | Int { value; _ } -> Int value 
  | UnOp { op; e; _ } -> eval_unop op (eval_expr env e) 
  | BinOp { op; e1; e2; _ } -> eval_binop op (eval_expr env e1) (eval_expr env e2)
  | If { e_cond; e_then; e_else; _ } -> begin 
    match eval_expr env e_cond with 
    | Bool true -> eval_expr env e_then 
    | Bool false -> eval_expr env e_else
    | _ -> raise TypeError
  end
  | Var { name; _ } -> lookup_var env name 
  | Let { name; def; body; _ } -> 
    let value = eval_expr env def in 
    let env' = add_var env name value in 
    eval_expr env' body 
  | App { func; args; _ } -> 
    let f = lookup_func env func in 
    let params = List.map (eval_expr env) args in 
    eval_app env f params 

and eval_unop op value = match op, value with 
  | Not, Bool b -> Bool (Bool.not b) 
  | Neg, Int i -> Int (- i) 
  | FNeg, Float f -> Float (-. f) 
  | _ -> raise TypeError 

and eval_binop op val1 val2 = match op, val1, val2 with 
  | And, Bool b1, Bool b2 -> Bool (b1 && b2) 
  | Or, Bool b1, Bool b2 -> Bool (b1 || b2) 
  | Implies, Bool b1, Bool b2 -> Bool ((Bool.not b1) || b2) 
  | Add, Int i1, Int i2 -> Int (i1 + i2) 
  | Sub, Int i1, Int i2 -> Int (i1 - i2) 
  | Mul, Int i1, Int i2 -> Int (i1 * i2) 
  | Div, Int i1, Int i2 -> Int (i1 / i2) 
  | Rem, Int i1, Int i2 -> Int (Int.rem i1 i2) 
  | FAdd, Float f1, Float f2 -> Float (f1 +. f2) 
  | FSub, Float f1, Float f2 -> Float (f1 -. f2) 
  | FMul, Float f1, Float f2 -> Float (f1 *. f2) 
  | FDiv, Float f1, Float f2 -> Float (f1 /. f2) 

  (* Should probably divide this into cases *)
  | Eq, a, b -> Bool (a = b) 
  | Neq, a, b -> Bool (a <> b)  
  | Leq, a, b -> Bool (a <= b) 
  | Lt, a, b -> Bool (a < b) 
  | Geq, a, b -> Bool (a >= b) 
  | Gt, a, b -> Bool (a > b) 
  | _ -> raise TypeError 

and eval_app _ _ _ = failwith "not yet implemented" 

and lookup_var env name = 
    let Env { values; _ } = env in 
    match List.assoc_opt name values with 
    | Some value -> value 
    | None -> raise (UnknownName name) 

and add_var env name value = 
    let Env { values; funcs } = env in 
    Env { values = (name, value) :: values; funcs } 

and lookup_func _ _ = failwith "not yet implemented" 
