open Type 
open Ast 

exception UnifyError of Type.t * Type.t 
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

let rec unify t1 t2 = match t1, t2 with 
  (* Basic types always unify with themselves *)
  | TBool, TBool | TUnit, TUnit | TInt, TInt | TFloat, TFloat -> () 

  (* If two type variables are the same *) 
  | TVar r1, TVar r2 when r1 == r2 -> () 

  (* If either t1 or t2 is an unbound type variable, try to unify it with 
     the other type 
  *)
  | TVar { contents = Some t1' }, _ -> unify t1' t2
  | _, TVar { contents = Some t2' } -> unify t1 t2' 

  (* If one of the types is yet unknown, alias it with the other *)
  | TVar ({ contents = None } as tv), ty 
  | ty, TVar ({ contents = None } as tv) -> 
    tv := Some ty 

  (* All else are type errors *)
  | _, _ -> raise (UnifyError (t1, t2))    

let ty_of_unop = function 
  | Not -> TBool 
  | Neg -> TInt 
  | FNeg -> TFloat 

let ty_of_binop = function 
  | And | Or | Implies -> TBool 
  | Add | Sub | Mul | Div | Rem -> TInt 
  | FAdd | FSub | FMul | FDiv -> TFloat 
  | Eq | Neq | Leq | Lt | Geq | Gt -> TBool 

let rec ty_expr env = function 
  | Unit _ -> TUnit 
  | Int _ -> TInt 
  | Bool _ -> TBool 
  | Float _ -> TFloat 
  | UnOp { op; e; _ } -> 
     let ty = ty_of_unop op in 
     unify ty (ty_expr env e); 
     ty 
  | BinOp { op; e1; e2; _ } -> 
    let ty = ty_of_binop op in 
    unify ty (ty_expr env e1); 
    unify ty (ty_expr env e2); 
    ty 
  | If { e_cond; e_then; e_else; ty; _ } -> 
    unify (ty_expr env e_cond) TBool; 
    let t_then = ty_expr env e_then in 
    let t_else = ty_expr env e_else in 
    unify t_then t_else;
    unify ty t_then; 
    t_then 
  | Var { name; _ } -> lookup_var env name 
  | Let { name; def; body; ty; _ } -> 
    unify ty (ty_expr env def); 
    ty_expr (add_var env name ty) body 
  | App { func; args; ty; _ } -> 
    let arg_tys, ret_ty = lookup_func env func in 
    let param_tys = List.map (ty_expr env) args in 
    List.iter2 unify arg_tys param_tys;
    unify ty ret_ty;   
    ret_ty
