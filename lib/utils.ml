let expr_of_string s = 
  let lbuf = Lexing.from_string s in 
  Parser.parse_expr Lexer.tokenize lbuf 
