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
  | App of { func : string; args : expr list; pos : pos }

and unop = Not | Neg | FNeg 
and binop = 
  | And | Or | Implies 
  | Add | Sub | Mul | Div | Rem 
  | FAdd | FSub | FMul | FDiv 
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

(* Conversion to S - Expression *)

open Sexplib0.Sexp 

let rec sexp_of_expr = function 
  | Unit _ -> Atom "()" 
  | Bool { value; _ } -> Atom (Bool.to_string value) 
  | Int { value; _ } -> Atom (Int.to_string value)
  | Float { value; _ } -> Atom (Float.to_string value) 
  | UnOp { op; e ; _ } -> List [sexp_of_unop op; sexp_of_expr e] 
  | BinOp { op; e1; e2; _ } -> List [sexp_of_binop op; sexp_of_expr e1; sexp_of_expr e2] 
  | If { e_cond; e_then; e_else; _ } -> List [Atom "if"; sexp_of_expr e_cond; sexp_of_expr e_then; sexp_of_expr e_else]
  | Var { name; _ } -> Atom name 
  | Let { name; def; body; _ } -> List [Atom "let"; Atom name; sexp_of_expr def; sexp_of_expr body] 
  | App { func; args; _ } -> List (Atom func :: List.map sexp_of_expr args) 

and sexp_of_unop op = Atom (match op with 
  | Not -> "!" 
  | Neg -> "-"
  | FNeg -> "-." 
)

and sexp_of_binop op = Atom (match op with 
  | And -> "&&"
  | Or -> "||"
  | Implies -> "=>"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Rem -> "%"
  | FAdd -> "+."
  | FSub -> "-."
  | FMul -> "*."
  | FDiv -> "/."  
  | Eq -> "="
  | Neq -> "!="
  | Leq -> "<="
  | Lt -> "<"
  | Geq -> ">="
  | Gt -> ">"
)

let sexp_of_func { inputs; ret_ty; body; _ } = 
  [
    List (List.map (fun (n, t) -> List [Atom n; Atom (Type.to_string t)]) inputs); 
    Atom (Type.to_string ret_ty); 
    sexp_of_expr body
  ]

let sexp_of_def = function 
  | Func (name, f) -> List (Atom "def-fun" :: Atom name :: sexp_of_func f) 
  | RecFunc fs -> List [Atom "def-rec"; List (List.map (fun (name, f) -> List (Atom name :: sexp_of_func f)) fs)]
  | Expr { name; body; ty; _ } -> List [Atom "def-val"; Atom name; Atom (Type.to_string ty); sexp_of_expr body] 

(* Printing *)
let indent = ref 2 

let fmt_expr ppf t = Sexplib0.Sexp.pp_hum_indent !indent ppf (sexp_of_expr t)  
let fmt_def ppf t = Sexplib0.Sexp.pp_hum_indent !indent ppf (sexp_of_def t) 
let fmt ppf t = 
  let _ = List.map (fmt_def ppf) t in 
  () 
