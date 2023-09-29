%{
    open Ast 
    open Ast_builder
%}


(* Unary operators *)
%token TK_NOT "not" 

(* Binary operators *)
%token TK_BAND "&&"
%token TK_BOR "||" 
%token TK_ADD "+" 
%token TK_SUB "-" 
%token TK_MUL "*" 
%token TK_DIV "/" 
%token TK_EQ "==" 
%token TK_NEQ "!=" 
%token TK_LT "<" 
%token TK_LEQ "<=" 
%token TK_GT ">" 
%token TK_GEQ ">=" 
%token TK_IMPLIES "=>"

(* Keywords *)
%token TK_FUN "fun" 
%token TK_REC "rec" 
%token TK_AND "and"
%token TK_LET "let" 
%token TK_IN "in" 
%token TK_VAL "val"
%token TK_IF "if" 
%token TK_THEN "then" 
%token TK_ELSE "else" 

(* Types *)
%token TK_TY_INT "int" 
%token TK_TY_BOOL "bool" 
%token TK_TY_UNIT "unit" 
%token TK_TY_FLOAT "float" 

(* Literals *) 
%token <int> TK_INT 
%token <float> TK_FLOAT 
%token <bool> TK_BOOL 
%token <string> TK_STRING 

(* Others *)
%token TK_LPAREN "("
%token TK_RPAREN ")"
%token TK_COLON ":" 
%token TK_COMMA "," 
%token TK_ASSIGN "=" 
%token TK_ARROW "->" 
%token TK_EOF "eof" 

%nonassoc "in" 
%left "else" 
%left "=>"
%left "||" 
%left "&&" 
%left "==" "!=" "<" "<=" ">" ">="
%right PREC_UNARY_NOT
%left "+" "-" 
%left "*" "/" 
%right PREC_UNARY_MINUS 

%start <Ast.t> parse
%start <Ast.expr> parse_expr

%%

parse:
    | d = def* "eof" { d }

parse_expr:
    | e = expr "eof" { e }

def: 
    | v = val_def { v }
    | f = func_def { f }

val_def: 
    | "val" nt = typed_name "=" e = expr { 
        let n, t = nt in 
        mk_expr ~pos:$loc ~ty:t n e 
    } 

func_def: 
    | "fun" nf = simple_fun { 
        let name, f = nf in 
        mk_func name f  
    }
    | "rec" fs = separated_nonempty_list("and", simple_fun) { 
        let names, fs = List.split fs in 
        mk_rec_func names fs 
    } 

simple_fun: 
    | name = TK_STRING "(" nl = separated_list(",", typed_name) ")" "->" 
        ret_ty = _type "=" body = expr { name, mk_func_def ~pos:$loc ~ty:ret_ty nl body }

simple_expr: 
    | "(" e = expr ")" { e }
    | "(" ")" { mk_unit ~pos:$loc () } 
    | b = TK_BOOL { mk_bool ~pos:$loc b }
    | i = TK_INT { mk_int ~pos:$loc i }
    | f = TK_FLOAT { mk_float ~pos:$loc f }
    | v = TK_STRING { mk_var ~pos:$loc v }

expr: 
    | e = simple_expr { e } 

    (* Unary operations *)
    | "not" e = expr %prec PREC_UNARY_NOT { mk_unop ~pos:$loc Not e } 
    | "-" e = expr %prec PREC_UNARY_MINUS { mk_unop ~pos:$loc Neg e }

    (* Binary operations *)
    | e1 = expr op = binop e2 = expr { mk_binop ~pos:$loc op e1 e2 }

    (* Conditional *)
    | "if" cond = expr "then" e_then = expr "else" e_else = expr { 
        mk_if ~pos:$loc cond e_then e_else
    }

    (* Function application *)
    | func = ident args = simple_expr+ { mk_app ~pos:$loc func args }

    (* Let binding *)
    | "let" nt = typed_name "=" def = expr "in" body = expr { 
        let n, t = nt in 
        mk_let ~pos:$loc ~ty:t n def body 
    }

%inline binop: 
    | "&&" { And }
    | "||" { Or }
    | "=>" { Implies }
    | "+" { Add }
    | "-" { Sub }
    | "*" { Mul }
    | "/" { Div }
    | "==" { Eq }
    | "!=" { Neq }
    | "<" { Lt }
    | "<=" { Leq }
    | ">" { Gt }
    | ">=" { Geq }

ident: 
    | id = TK_STRING { id } 

typed_name: 
    | name = TK_STRING { name, Type.TUnknown } 
    | name = TK_STRING ":" ty = _type { name, ty }

_type: 
    | "int" { Type.TInt }
    | "bool" { Type.TBool }
    | "float" { Type.TFloat }
    | "unit" { Type.TUnit }
