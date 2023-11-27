open Lexing

(*type token =
| Symbol of string
| Word of string
| Number of string

let string_of_token token =
    match token with
    | Symbol str -> str
    | Word str -> str
    | Number str -> str

let () = print_endline (string_of_token (Symbol "bruh"))
let () = Symbol "bruh" |> string_of_token |> print_endline*)

let () =
  let input_channel = 
    if Array.length(Sys.argv) > 1 then open_in Sys.argv.(1) else stdin in
    
  let lexbuf = Lexing.from_channel ~with_positions:true input_channel in
  
  try
    (*let result = Interpreter.eval_expr (Parser.program Lexer.token lexbuf) in
    Printf.printf "Parsed result: %s\n" (match result with | Int x -> string_of_int x | Bool x -> string_of_bool x | _ -> "balls")*)
    (*let f elem =
      Interpreter.eval_statement (elem)
    in List.iter f (Parser.program Lexer.token lexbuf)*)
    Printf.printf "Parsing %s:\n" Sys.argv.(1);
    let statements = Parser.program Lexer.token lexbuf in
    Printf.printf "Typechecking %s:\n" Sys.argv.(1);
    let _ = match Typechecker.check_statements (Typechecker.TypeChk.empty ()) statements Ast.TUnit with
    | Ok _ -> 
      Printf.printf "Interpreting %s:\n" Sys.argv.(1);
      let toplevel = Interpreter.eval_statements statements State.PrgmSt.empty in
      Interpreter.eval_statement toplevel (Ast.Eval (Ast.FuncCall (Ident "main", [])))
    | Error err -> raise err
    in ()

  with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    Printf.printf "Syntax error at line %d, column %d\n"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
