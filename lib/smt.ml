open Z3 
open Substitution

let trans_unop ctx op e = match op with 
  | Ast.Not -> Boolean.mk_not ctx e 
  | Neg -> Arithmetic.mk_unary_minus ctx e 

let trans_binop ctx op e1 e2 = match op with 
  | Ast.And -> Boolean.mk_and ctx [e1; e2] 
  | Or -> Boolean.mk_or ctx [e1; e2] 
  | Implies -> Boolean.mk_implies ctx e1 e2 
  | Add -> Arithmetic.mk_add ctx [e1; e2] 
  | Sub -> Arithmetic.mk_sub ctx [e1; e2] 
  | Mul -> Arithmetic.mk_mul ctx [e1; e2] 
  | Div -> Arithmetic.mk_div ctx e1 e2 
  | Rem -> Arithmetic.Integer.mk_rem ctx e1 e2 
  | Eq -> Boolean.mk_eq ctx e1 e2 
  | Neq -> Boolean.mk_not ctx (Boolean.mk_eq ctx e1 e2) 
  | Leq -> Arithmetic.mk_le ctx e1 e2 
  | Lt -> Arithmetic.mk_lt ctx e1 e2 
  | Geq -> Arithmetic.mk_ge ctx e1 e2 
  | Gt -> Arithmetic.mk_gt ctx e1 e2 

let int_sort ctx = Arithmetic.Integer.mk_sort ctx 

let sort_of_ty ctx = function 
  | Type.TUnit -> assert false 
  | TInt -> Arithmetic.Integer.mk_sort ctx 
  | TBool -> Boolean.mk_sort ctx 
  | TFloat -> Arithmetic.Real.mk_sort ctx 
  | TUnknown -> assert false 

let rec trans_expr ctx = function 
  | Unit -> failwith "don't know yet" 
  | Int i -> Arithmetic.Integer.mk_numeral_i ctx i 
  | Float f -> Arithmetic.Real.mk_numeral_s ctx (string_of_float f) 
  | Bool b -> Boolean.mk_val ctx b 
  | UnOp (_, op, e) -> trans_unop ctx op (trans_expr ctx e) 
  | BinOp (_, op, e1, e2) -> 
    trans_binop ctx op (trans_expr ctx e1) (trans_expr ctx e2)
  | If (_, c, t, e) -> 
    Boolean.mk_ite ctx (trans_expr ctx c) (trans_expr ctx t) (trans_expr ctx e)  
  | Var (ty, name) -> Expr.mk_const_s ctx name (sort_of_ty ctx ty) 
  | _ -> failwith ""
