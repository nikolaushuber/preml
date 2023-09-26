{
    open Parser 
    
    exception LexingError of string 
}

let space = [' ' '\t' '\r'] 
let newline = ('\013'* '\010')
let digit = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let alpha = (lower | upper)
let identifier = ('_' | alpha ) (alpha | digit | "_")*
let integer = digit+ 
let float = digit+ "." digit+ 


rule tokenize = parse 
    | space { tokenize lexbuf }
    | newline { Lexing.new_line lexbuf; tokenize lexbuf }
    | "(*" { read_comment lexbuf }

    (* Unary operators *)
    | "not" { TK_NOT }

    (* Binary operators *)
    | "&&" { TK_BAND }
    | "||" { TK_BOR }
    | "+" { TK_ADD }
    | "-" { TK_SUB }
    | "*" { TK_MUL }
    | "/" { TK_DIV }
    | "+." { TK_FADD }
    | "-." { TK_FSUB }
    | "*." { TK_FMUL }
    | "/." { TK_FDIV }
    | "==" { TK_EQ }
    | "!=" { TK_NEQ }
    | "<" { TK_LT }
    | "<=" { TK_LEQ }
    | ">" { TK_GT }
    | ">=" { TK_GEQ }
    | "=>" { TK_IMPLIES }

    (* Keywords *)
    | "fun" { TK_FUN }
    | "rec" { TK_REC }
    | "and" { TK_AND }
    | "let" { TK_LET }
    | "in" { TK_IN }
    | "val" { TK_VAL } 
    | "if" { TK_IF }
    | "then" { TK_THEN }
    | "else" { TK_ELSE }
    | "->" { TK_ARROW }

    (* Types *)
    | "int" { TK_TY_INT }
    | "bool" { TK_TY_BOOL }
    | "float" { TK_TY_FLOAT }
    | "unit" { TK_TY_UNIT }
    
    (* Constants *)
    | "true" { TK_BOOL true }
    | "false" { TK_BOOL false }
    | integer as i { TK_INT (int_of_string i) }
    | float as f { TK_FLOAT (float_of_string f) }
    | identifier as id { TK_STRING id }

    (* Others *)
    | "(" { TK_LPAREN }
    | ")" { TK_RPAREN }
    | "=" { TK_ASSIGN }
    | ":" { TK_COLON }
    | "." { TK_DOT }
    | "," { TK_COMMA }
    | eof { TK_EOF } 

and read_comment = parse 
    | "*)" { tokenize lexbuf }
    | newline { Lexing.new_line lexbuf; read_comment lexbuf }
    | eof { raise (LexingError "Reached end of file inside comment") }
    | _ { read_comment lexbuf }
