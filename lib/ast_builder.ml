open Ast 
open Type 

let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos 

let mk_unit ?(pos = dummy_pos) () = Unit { pos } 
let mk_bool ?(pos = dummy_pos) value = Bool { pos; value } 
let mk_int ?(pos = dummy_pos) value = Int { pos; value }
let mk_float ?(pos = dummy_pos) value = Float { pos; value } 
let mk_unop ?(pos = dummy_pos) ?(ty = TUnknown) op e = UnOp { pos; op; e; ty } 
let mk_binop ?(pos = dummy_pos) ?(ty = TUnknown) op e1 e2 = 
  BinOp { pos; op; e1; e2; ty } 
let mk_if ?(pos = dummy_pos) ?(ty = TUnknown) e_cond e_then e_else = 
  If { pos; ty; e_cond; e_then; e_else }
let mk_var ?(pos = dummy_pos) ?(ty = TUnknown) name = Var { pos; name; ty } 
let mk_let ?(pos = dummy_pos) ?(ty = TUnknown) name def body = 
  Let { pos; ty; name; def; body } 
let mk_app ?(pos = dummy_pos) ?(ty = TUnknown) func args = 
  App { pos; func; args; ty }


let mk_expr ?(pos = dummy_pos) ?(ty = TUnknown) name body = 
  Expr { name; body; pos; ty }
let mk_func_def ?(pos = dummy_pos) ?(ty = TUnknown) inputs body = 
  { inputs; ret_ty = ty; body; pos }
let mk_func name def = Func (name, def) 
let mk_rec_func names defs = 
  RecFunc (List.map2 (fun name def -> name, def) names defs)
