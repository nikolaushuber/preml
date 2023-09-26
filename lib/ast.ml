type pos = Lexing.position * Lexing.position
type ty = Type.t 

type expr = 
  | Unit of { pos : pos } 
  | Bool of { value : bool; pos : pos }
  | Int of { value : int; pos : pos } 
  | Float of { value : float; pos : pos } 
  | UnOp of { op : unop; e : expr; pos : pos } 
  | BinOp of { op: binop; e1 : expr; e2 : expr; pos : pos }  
  | If of { pos : pos; ty : ty; e_cond : expr; e_then : expr; e_else : expr }
  | Var of { name : string; pos : pos } 
  | Let of { name : string; pos : pos; ty : ty; def : expr; body : expr } 
  | App of { func : Ident.t; args : expr list; pos : pos }

and unop = Not | Neg | FNeg 
and binop = 
  | And | Or | Implies 
  | Add | Sub | Mul | Div | Rem 
  | FAdd | FSub | FMul | FDiv 
  | Eq | Neq | Leq | Lt | Geq | Gt 

type func = { 
  name : string; 
  inputs : (string * ty) list; 
  ret_ty : ty; 
  body : expr; 
  pos : pos
}

type def = 
  | Func of func 
  | RecFunc of func list 
  | Expr of { name : string; body : expr; pos : pos; ty : ty }

type t = def list 
