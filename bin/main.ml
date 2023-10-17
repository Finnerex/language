open Lexing

let () = print_endline "mehhhh"

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
    
  let lexbuf = Lexing.from_channel input_channel in
  
  try
    (*let result = Interpreter.eval_expr (Parser.program Lexer.token lexbuf) in
    Printf.printf "Parsed result: %s\n" (match result with | Int x -> string_of_int x | Bool x -> string_of_bool x | _ -> "balls")*)
    (*let f elem =
      Interpreter.eval_statement (elem)
    in List.iter f (Parser.program Lexer.token lexbuf)*)
    Interpreter.eval_statements (Parser.program Lexer.token lexbuf) Interpreter.PrgmSt.empty
    
  with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    Printf.printf "Syntax error at line %d, column %d\n"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
