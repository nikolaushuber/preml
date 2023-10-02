let expr_of_string s = 
  let lbuf = Lexing.from_string s in 
  Parser.parse_expr Lexer.tokenize lbuf 

let ast_of_string s = 
  let lbuf = Lexing.from_string s in 
  Parser.parse Lexer.tokenize lbuf 

let ast_of_file file = 
  let ic = open_in file in 
  let lbuf = Lexing.from_channel ic in 
  Parser.parse Lexer.tokenize lbuf 
