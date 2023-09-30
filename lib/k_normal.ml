type id = string 

type expr = 
  | Unit 
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
  | Func of (string * ((id * Type.t) list * Type.t * expr))
  | RecFunc of (string * ((id * Type.t) list * Type.t * expr)) list 
  | Expr of string * Type.t * expr 
  

(* 
module S = Set.Make(String) 

let rec free_vars = function 
  | Unit | Int _ | Float _ | Bool _ -> S.empty 

  | UnOp (_, x) 
  | Var x -> S.singleton x 

  | BinOp (_, x, y) -> S.of_list [x; y] 

  | If (x, e1, e2)  
  | Let (x, _, e1, e2) -> S.add x (S.union (free_vars e1) (free_vars e2)) 

  | App (f, args) -> 
    List.fold_left (fun s arg -> S.add arg s) (S.singleton f) args  *)

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

let rec trans_expr env = function 
  | Ast.Unit _ -> Unit, Type.TUnit 
  | Bool { value; _ } -> Bool value, TBool 
  | Int { value; _ } -> Int value, TInt 
  | Float { value; _ } -> Float value, TFloat 
  | UnOp { op; e; ty; _ } -> 
    ins_let (trans_expr env e) (fun x -> UnOp (op, x), ty)
  | BinOp { op; e1; e2; ty; _ } -> 
    ins_let (trans_expr env e1) 
      (fun x -> ins_let (trans_expr env e2) 
        (fun y -> BinOp (op, x, y), ty))
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

let trans_func f = 
  let Ast.{inputs; ret_ty; body; _ } = f in 
  counter := 0;   (* TODO see above *)
  let body', _ = trans_expr inputs body in 
  inputs, ret_ty, body' 

let trans_def = function 
  | Ast.Func (name, f) -> Func (name, trans_func f)
  | RecFunc nfs -> 
    let nfs' = List.map (fun (name, f) -> name, trans_func f) nfs in 
    RecFunc nfs' 
  | Expr { name; body; ty; _ } -> Expr (name, ty, fst (trans_expr [] body))

let f = List.map trans_def 

(* Conversion to S - Expression *) 

open Sexplib0.Sexp 

let rec sexp_of_expr = function 
  | Unit -> Atom "()" 
  | Bool b -> Atom (string_of_bool b) 
  | Int i -> Atom (string_of_int i) 
  | Float f -> Atom (string_of_float f) 
  | UnOp (op, id) -> List [Ast.sexp_of_unop op; Atom id] 
  | BinOp (op, id1, id2) -> List [Ast.sexp_of_binop op; Atom id1; Atom id2] 
  | If (id, t, e) -> List [Atom id; sexp_of_expr t; sexp_of_expr e] 
  | Var id -> Atom id 
  | Let (n, _, e1, e2) -> List [Atom "let"; List [Atom n; sexp_of_expr e1]; sexp_of_expr e2] 
  | App (f, args) -> List (Atom f :: List.map (fun f -> Atom f) args)

let sexp_of_func f = 
  let name, (args, _, e) = f in 
  [
    Atom name; 
    List (List.map (fun (n, t) -> List [Atom n; Atom (Type.to_string t)]) args); 
    sexp_of_expr e 
  ]

let sexp_of_def = function 
  | Func f -> List (Atom "def-fun" :: sexp_of_func f) 
  | RecFunc fs -> 
    List [Atom "def-rec"; List (List.map (fun x -> List (sexp_of_func x)) fs)] 
  | Expr (name, _, e) -> List [Atom "def-val"; Atom name; sexp_of_expr e] 

(* Printing *)
let indent = ref 2 

let fmt_expr ppf t = Sexplib0.Sexp.pp_hum_indent !indent ppf (sexp_of_expr t)  
let fmt_def ppf t = Sexplib0.Sexp.pp_hum_indent !indent ppf (sexp_of_def t) 
let fmt ppf t = 
  let _ = List.map (fmt_def ppf) t in 
  () 
