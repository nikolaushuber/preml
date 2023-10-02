type expr = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | Float of float 
  | UnOp of Ast.unop * expr 
  | BinOp of Ast.binop * expr * expr 
  | If of expr * expr * expr 
  | Var of expr 
  | App of string * expr list 

