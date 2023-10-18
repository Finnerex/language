{
  open Parser
  let string_buf = Buffer.create 256
}

let blank = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let ident = '_'* + ['A'-'Z' 'a'-'z'] + ['A'-'Z' 'a'-'z' '_' '0'-'9']*

rule token = parse
| blank+     { token lexbuf }
| digit+     { INT_LIT (int_of_string(Lexing.lexeme lexbuf)) }

| '='        { ASSIGN_EQUALS }

| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIV }

| '('        { LPAREN }
| ')'        { RPAREN }

| '{'        { LSCOPE }
| '}'        { RSCOPE }

| "if"       { IF }

| "true"     { BOOL_LIT true }
| "false"    { BOOL_LIT false }

| "=="       { BOOL_EQUALS }
| "&&"       { AND }
| "||"       { OR }
| '!'        { NOT }

| '?'        { TERNARY_QUESTIONMARK }
| ':'        { TERNARY_COLON }

| "println"  { PRINTLN }
| "print"    { PRINT }

| ';'        { ENDLINE }
| eof        { EOF }
| ";;"       { EOF }

| ident      { IDENT (Lexing.lexeme lexbuf) }
| '"'        { Buffer.clear string_buf;
               string lexbuf;
               STRING_LIT (Buffer.contents string_buf) }

| _          { failwith (Printf.sprintf "unexpected character: %s" (Lexing.lexeme lexbuf)) }

and string = parse
| '"' { () }
| _ as c { Buffer.add_char string_buf c;
           string lexbuf }
(* add backslash escapes *)
