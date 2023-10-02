type id = string

type expr =
    Unit
  | Int of int
  | Bool of bool
  | Float of float
  | UnOp of Ast.unop * id
  | BinOp of Ast.binop * id * id
  | If of id * expr * expr
  | Var of id
  | Let of id * Type.t * expr * expr
  | App of id * id list

type def =
    Func of (id * ((id * Type.t) list * Type.t * expr))
  | RecFunc of (id * ((id * Type.t) list * Type.t * expr)) list
  | Expr of id * Type.t * expr

val trans_expr : (string * Type.t) list -> Ast.expr -> expr * Type.t
val trans_func : Ast.func -> (string * Type.t) list * Type.t * expr
val trans_def : Ast.def -> def
val f : Ast.def list -> def list

val fmt_expr : Format.formatter -> expr -> unit
[@@ocaml.toplevel_printer]

val fmt_def : Format.formatter -> def -> unit
[@@ocaml.toplevel_printer]

val fmt : Format.formatter -> def list -> unit
[@@ocaml.toplevel_printer]
