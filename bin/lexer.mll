{
  open Parser
  let string_buf = Buffer.create 256
}

let blank = [' ' '\t']
let newline = "\r\n" | '\n'
let digit = ['0'-'9']
let ident = '_'* ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*

rule token = parse
| blank+     { token lexbuf }
| newline   { Lexing.new_line lexbuf; token lexbuf }
| digit+     { INT_LIT (int_of_string(Lexing.lexeme lexbuf)) }

| "//"       { line_comment lexbuf |> token }
| "/*"       { block_comment lexbuf |> token }

| '='        { ASSIGN_EQUALS }

| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIV }
| '%'        { MODULO }

| "++"       { INCREMENT }

| '('        { LPAREN }
| ')'        { RPAREN }

| '{'        { LCURLY }
| '}'        { RCURLY }

| "if"       { IF }
| "else"     { ELSE }

| "while"    { WHILE }
| "for"      { FOR }

| "true"     { BOOL_LIT true }
| "false"    { BOOL_LIT false }

| "=="       { BOOL_EQUALS }
| "&&"       { AND }
| "||"       { OR }
| '!'        { NOT }
| "<="       { LESS_EQ }
| '<'        { LESS }
| ">="       { GREATER_EQ }
| '>'        { GREATER }

| '?'        { TERNARY_QUESTIONMARK }
| ':'        { TERNARY_COLON }

| ','        { COMMA }

| "return"   { RETURN }

| "println"  { PRINTLN }
| "print"    { PRINT }

| "systime"  { SYSTIME }

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

and line_comment = parse (* wanted to do both comment types using an arg but it wasnt working so idk *)
| '\n'  { lexbuf }
| _     { line_comment lexbuf }

and block_comment = parse
| "*/"  { lexbuf }
| _     { block_comment lexbuf }
