type expr = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | Float of float 
  | UnOp of Type.t * Ast.unop * expr 
  | BinOp of Type.t * Ast.binop * expr * expr 
  | If of Type.t * expr * expr * expr 
  | Var of Type.t * string 
  | App of string * expr list 

type def = 
  | Func of (string * ((string * Type.t) list * Type.t * expr))
  | RecFunc of (string * ((string * Type.t) list * Type.t * expr)) list 
  | Expr of string * Type.t * expr 

let rec inline_expr env = function 
  | Ast.Unit _ -> Unit 
  | Int { value; _ } -> Int value 
  | Float { value; _ } -> Float value 
  | Bool { value; _ } -> Bool value 
  | UnOp { op; e; ty; _ } -> UnOp (ty, op, inline_expr env e) 
  | BinOp { op; e1; e2; ty; _ } -> 
    BinOp (ty, op, inline_expr env e1, inline_expr env e2) 
  | If { ty; e_cond; e_then; e_else; _ } -> 
    If (ty, inline_expr env e_cond, inline_expr env e_then, 
      inline_expr env e_else) 
  | Var { name; ty; _ } -> 
    if List.mem_assoc name env then List.assoc name env else Var (ty, name) 
  | Let { name; def; body; _ } -> 
    let in_def = inline_expr env def in 
    inline_expr ((name, in_def) :: env) body 
  | App { func; args; _ } -> App (func, List.map (inline_expr env) args) 
  
let inline_func f = 
  let Ast.{ inputs; ret_ty; body; _ } = f in 
  inputs, ret_ty, inline_expr [] body

let inline_def = function 
  | Ast.Func (name, f) -> Func (name, inline_func f)
  | RecFunc nfs -> RecFunc (List.map (fun (n, f) -> n, inline_func f) nfs) 
  | Expr { name; body; ty; _ } -> Expr (name, ty, inline_expr [] body) 

let f = List.map inline_def 
