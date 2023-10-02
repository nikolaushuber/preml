type pos = Lexing.position * Lexing.position
type ty = Type.t 

type expr = 
  | Unit of { pos : pos } 
  | Bool of { value : bool; pos : pos }
  | Int of { value : int; pos : pos } 
  | Float of { value : float; pos : pos } 
  | UnOp of { op : unop; e : expr; pos : pos; mutable ty : ty } 
  | BinOp of { op: binop; e1 : expr; e2 : expr; pos : pos; mutable ty : ty }  
  | If of { pos : pos; mutable ty : ty; e_cond : expr; e_then : expr; e_else : expr }
  | Var of { name : string; pos : pos } 
  | Let of { name : string; pos : pos; mutable ty : ty; def : expr; body : expr } 
  | App of { func : string; args : expr list; pos : pos; mutable ty : ty }

and unop = Not | Neg 
and binop = 
  | And | Or | Implies 
  | Add | Sub | Mul | Div | Rem 
  | Eq | Neq | Leq | Lt | Geq | Gt 

type func = { 
  inputs : (string * ty) list; 
  ret_ty : ty; 
  body : expr; 
  pos : pos
}

type def = 
  | Func of string * func 
  | RecFunc of (string * func) list 
  | Expr of { name : string; body : expr; pos : pos; ty : ty }

type t = def list 


val sexp_of_expr : expr -> Sexplib0.Sexp.t
val sexp_of_unop : unop -> Sexplib0.Sexp.t
val sexp_of_binop : binop -> Sexplib0.Sexp.t
val sexp_of_func : func -> Sexplib0.Sexp.t list
val sexp_of_def : def -> Sexplib0.Sexp.t

val indent : int ref

val fmt_unop : Format.formatter -> unop -> unit 

val fmt_binop : Format.formatter -> binop -> unit 
val fmt_expr : Format.formatter -> expr -> unit
[@@ocaml.toplevel_printer]

val fmt_def : Format.formatter -> def -> unit
[@@ocaml.toplevel_printer]

val fmt : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]
