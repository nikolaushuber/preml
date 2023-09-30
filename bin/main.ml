open Preml 
open Cmdliner 

let load_t = 
  let doc = "List of PreML files to load." in 
  Arg.(value & opt (list file) [] & info ["l"; "load"] ~doc) 

let expr_t = 
  let doc = "Expressions to evaluate." in 
  Arg.(value & pos 0 string "()" & info [] ~doc)

let eval_e files expr = 
  let asts = List.map (fun file -> 
    let ic = open_in file in 
    let lbuf = Lexing.from_channel ic in 
    Parser.parse Lexer.tokenize lbuf 
    ) files in 
  let _ = List.fold_left Typing.f Typing.empty_env asts in 
  let env = List.fold_left Eval.eval Eval.empty_env asts in 
  let e = Utils.expr_of_string expr in 
  let value = Eval.eval_expr env e in 
  print_endline (Eval.string_of_value value) 

let eval_t = Term.(const eval_e $ load_t $ expr_t) 
let eval_cmd = Cmd.(v (info "eval") eval_t) 

let cmd = Cmd.(group (info "preml") [eval_cmd])

let () = exit (Cmd.eval cmd)
