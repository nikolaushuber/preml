(* type id = string 

module S = Set.Make(String) 

type k_expr = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | Float of float 
  | UnOp of Ast.unop * id 
  | BinOp of Ast.binop * id * id 
  | If of id * k_expr * k_expr 
  | Var of id 
  | Let of id * Type.t * k_expr * k_expr 
  | App of id * id list 

let rec free_vars = function 
  | Unit | Int _ | Float _ | Bool _ -> S.empty 

  | UnOp (_, x) 
  | Var x -> S.singleton x 

  | BinOp (_, x, y) -> S.of_list [x; y] 

  | If (x, e1, e2)  
  | Let (x, _, e1, e2) -> S.add x (S.union (free_vars e1) (free_vars e2)) 

  | App (f, args) -> 
    List.fold_left (fun s arg -> S.add arg s) (S.singleton f) args 

(* TODO: Find a version without global side effect *)
let counter = ref 0     
let gen_tmp t = 
  incr counter; 
  Printf.sprintf "tmp%s%d" (Type.to_string t) !counter 

(* The following algorithms are inspired by the MinCaml compiler *)

let ins_let (e, t) k = match e with 
  | Var x -> k x 
  | _ -> 
      let x = gen_tmp t in 
      let e', t' = k x in 
      Let (x, t, e, e'), t' 

open Type 

let rec trans_expr env = function 
  | Ast.Unit _ -> Unit, TUnit 
  | Bool { value; _ } -> Bool value, TBool 
  | Int { value; _ } -> Int value, TInt 
  | Float { value; _ } -> Float value, TFloat 
  | UnOp { op; e; _ } -> 
    ins_let (trans_expr env e) (fun x -> UnOp (op, x), Typing.ty_of_unop op)
  | BinOp { op; e1; e2; _ } -> 
    ins_let (trans_expr env e1) 
      (fun x -> ins_let (trans_expr env e2) 
        (fun y -> BinOp (op, x, y), Typing.ty_of_binop op))
  | If { e_cond; e_then; e_else; ty; _ } -> 
    ins_let (trans_expr env e_cond) (fun x -> 
      let e1, _ = trans_expr env e_then in 
      let e2, _ = trans_expr env e_else in
      If (x, e1, e2), ty)
  | Var { name; _ } -> Var name, List.assoc name env 
  | Let { name; ty; def; body; _ } -> 
    let e1, t1 = trans_expr env def in 
    let e2, t2 = trans_expr ((name, t1) :: env) body in 
    Let (name, ty, e1, e2), t2 
  | App { func; args; ty; _ } -> 
    let rec bind xs = function 
      | [] -> App (func, xs), ty 
      | e2 :: e2s -> ins_let (trans_expr env e2) (fun x -> bind (xs @ [x]) e2s) 
    in
    bind [] args  

let trans_func env f = 
  let Ast.{inputs; ret_ty; body; _ } = f in 
  let env' = inputs @ env in 
  counter := 0;   (* TODO see above *)
  let body', _ = trans_expr env' body in 
  inputs, ret_ty, body' 
  
 *)
