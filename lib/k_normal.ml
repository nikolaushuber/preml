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

(* Printing *)

open Format 

let is_let = function 
  | Let _ -> true 
  | _ -> false 

let rec fmt_expr ppf = function 
  | Unit -> pp_print_string ppf "()" 
  | Int i -> pp_print_int ppf i 
  | Bool b -> pp_print_bool ppf b 
  | Float f -> pp_print_float ppf f 
  | UnOp (op, e) -> fprintf ppf "(%a %s)" Ast.fmt_unop op e 
  | BinOp (op, e1, e2) -> fprintf ppf "(%s %a %s)" e1 Ast.fmt_binop op e2 
  | If (c, t, e) -> 
      pp_open_box ppf 0; 
      pp_print_string ppf "if"; 
      pp_print_space ppf (); 
      pp_print_string ppf c; 
      pp_print_space ppf (); 
      pp_print_string ppf "then"; 
      pp_print_space ppf (); 
      pp_open_hvbox ppf 2; 
      fmt_expr ppf t; 
      pp_close_box ppf (); 
      pp_print_space ppf (); 
      pp_print_string ppf "else"; 
      pp_print_space ppf (); 
      pp_open_hvbox ppf 2; 
      fmt_expr ppf e; 
      pp_close_box ppf (); 
      pp_close_box ppf () 
  | Var v -> pp_print_string ppf v 
  | App (f, args) -> 
      fprintf ppf "(%s@ %a)" f (pp_print_list pp_print_string) args 
  | Let (name, _, e1, e2) -> 
      if is_let e1 then 
        fprintf ppf "@[<v 2>let %s =@;%a@]@;in@;%a" 
          name 
          fmt_expr e1 
          fmt_expr e2  
      else
        fprintf ppf "let %s = %a in@;%a" 
          name 
          fmt_expr e1 
          fmt_expr e2 

let fmt_args ppf args = 
  let fmt_arg ppf (name, ty) = fprintf ppf "(%s : %a)" name Type.fmt ty in 
  pp_print_list fmt_arg ppf args 

let fmt_func ppf f = 
  let name, (args, _, body) = f in 
  fprintf ppf "@[<v 2>fun %s (%a) =@;%a@]" 
      name 
      fmt_args args 
      fmt_expr body 

let fmt_def ppf = function 
  | Func f -> fmt_func ppf f
  | RecFunc fs -> pp_print_list fmt_func ppf fs 
  | Expr (name, _, e) -> 
    if is_let e then 
      fprintf ppf "@[<v 2>val %s =@;%a@]" name fmt_expr e 
    else 
      fprintf ppf "val %s = %a" name fmt_expr e 

let dn_sep ppf () = fprintf ppf "@;@;"

let fmt ppf t = 
  fprintf ppf "@[<v 0>%a@]" (pp_print_list ~pp_sep:dn_sep fmt_def) t 
