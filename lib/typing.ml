open Type 
open Ast 

exception UnifyError of Type.t * Type.t 
exception TypeError 
exception ArityError of string 
exception NameError of string 

type ('a, 'b) t = Env of {
  values : (string * Type.t) list; 
  funcs : (string * (Type.t list * Type.t)) list; 
}

let empty_env = Env { values = []; funcs = [] } 

let lookup_var env name = 
  let Env { values; _ } = env in 
  match List.assoc_opt name values with 
  | Some value -> value 
  | None -> raise (NameError name) 

let add_var env name value = 
  let Env { values; funcs } = env in 
  Env { values = (name, value) :: values; funcs } 

let lookup_func env name = 
  let Env { funcs; _ } = env in 
  match List.assoc_opt name funcs with 
  | None -> raise (NameError name) 
  | Some f -> f 

let add_func env name func = 
  let Env { values; funcs } = env in 
  Env { values; funcs = (name, func) :: funcs }    

let check_unop op ty = match op, ty with 
  | Not, TBool -> TBool 
  | Neg, TInt | Neg, TFloat -> ty 
  | _ -> raise TypeError 
  
let check_binop op t1 t2 = match op, t1, t2 with 
  | And, TBool, TBool 
  | Or, TBool, TBool 
  | Implies, TBool, TBool -> TBool 
  | Add, TInt, TInt | Add, TFloat, TFloat 
  | Sub, TInt, TInt | Sub, TFloat, TFloat 
  | Mul, TInt, TInt | Mul, TFloat, TFloat 
  | Div, TInt, TInt | Div, TFloat, TFloat 
  | Rem, TInt, TInt -> t1
  | Eq, TInt, TInt | Eq, TFloat, TFloat | Eq, TBool, TBool 
  | Neq, TInt, TInt | Neq, TFloat, TFloat | Neq, TBool, TBool 
  | Leq, TInt, TInt | Leq, TFloat, TFloat 
  | Lt, TInt, TInt | Lt, TFloat, TFloat 
  | Geq, TInt, TInt | Geq, TFloat, TFloat 
  | Gt, TInt, TInt | Gt, TFloat, TFloat -> TBool 
  | _ -> raise TypeError 
  

let rec ty_expr env = function 
  | Unit _ -> TUnit 
  | Int _ -> TInt 
  | Bool _ -> TBool 
  | Float _ -> TFloat 
  | UnOp ({ op; e; _ } as _unop) -> 
     let ty = ty_expr env e in 
     let ty = check_unop op ty in 
     _unop.ty <- ty; 
     ty
  | BinOp ({ op; e1; e2; _ } as _binop) -> 
    let t1 = ty_expr env e1 in 
    let t2 = ty_expr env e2 in 
    let t = check_binop op t1 t2 in 
    _binop.ty <- t; 
    t
  | If ({ e_cond; e_then; e_else; _ } as _if) -> 
    let t_cond = ty_expr env e_cond in 
    if t_cond <> TBool then raise TypeError; 
    let t_then = ty_expr env e_then in 
    let t_else = ty_expr env e_else in 
    if t_then <> t_else then raise TypeError; 
    _if.ty <- t_then;
    t_then 
  | Var { name; _ } -> lookup_var env name 
  | Let ({ name; def; body; _ } as _let) -> 
    let t_def = ty_expr env def in  
    let _ = ty_expr (add_var env name t_def) body in 
    _let.ty <- t_def; 
    t_def  
  | App { func; args; _ } -> 
    let arg_tys, ret_ty = lookup_func env func in 
    let param_tys = List.map (ty_expr env) args in 
    List.iter2 (fun t1 t2 -> 
      if t1 <> t2 then raise TypeError
    ) arg_tys param_tys;  
    ret_ty
