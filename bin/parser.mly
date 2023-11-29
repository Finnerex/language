%{
  open Ast
  open Lexing

  let convert_pos (p:Lexing.position) = { line_num = p.pos_lnum; char_num = p.pos_cnum - p.pos_bol; }
  let mexpr expr (s, e) = { expr = expr; pos_start = convert_pos s; pos_end = convert_pos e }
  let mstmt stmt (s, e) = { stmt = stmt; pos_start = convert_pos s; pos_end = convert_pos e }
  let midnt ident (s, e) = { ident = ident; pos_start = convert_pos s; pos_end = convert_pos e }
%}

(* Litterals *)
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string> IDENT

(*%token <char> CHAR_LIT
%token <float> FLOAT_LIT *)

(*%token TYPE*)

%token ASSIGN_EQUALS

%token COMMA

%token LPAREN RPAREN
%token LCURLY RCURLY

(* Arithmetic operators *)
%token PLUS MINUS
%token TIMES DIV MODULO

%token INCREMENT

(* Boolean opperators *)
%token AND OR NOT BOOL_EQUALS
%token GREATER LESS
%token GREATER_EQ LESS_EQ

%token TERNARY_QUESTIONMARK TERNARY_COLON

%token IF ELSE
%token WHILE FOR

%token RETURN

%token SYSTIME

%token PRINT
%token PRINTLN

%token ENDLINE
%token EOF

(* precedents, lower prec comes first *)
%left TERNARY_QUESTIONMARK TERNARY_COLON
%right AND
%right OR
%left BOOL_EQUALS
%nonassoc GREATER GREATER_EQ LESS LESS_EQ
%left PLUS MINUS
%left TIMES DIV MODULO
%right NOT
%nonassoc INCREMENT


(* actual parsing *)

%start program
%type <Ast.stmt_info list> program

%%

program:
| list(toplevel_statement) EOF
  { $1 }
;

toplevel_statement:
| ident ident ASSIGN_EQUALS expression ENDLINE
  { mstmt (Assign ($1, $2, $4)) $loc }
| ident ident LPAREN separated_list(COMMA, param_def) RPAREN LCURLY list(body_statement) RCURLY
  { mstmt (FuncDef ($1, $2, $4, $7)) $loc }

body_statement:
| incomplete_body_statement ENDLINE
  { $1 }

| RETURN expression ENDLINE
  { mstmt (Return $2) $loc }

| RETURN ENDLINE
  { mstmt (Return (mexpr Unit $loc($1))) $loc }
  
| IF LPAREN expression RPAREN LCURLY list(body_statement) RCURLY list(elseif)
  { mstmt (If (($3, $6) :: $8)) $loc }

| IF LPAREN expression RPAREN incomplete_body_statement ENDLINE list(elseif) (* can only have one one-line statement *)
  { mstmt (If (($3, [$5]) :: $7)) $loc }

| WHILE LPAREN expression RPAREN LCURLY list(body_statement) RCURLY
  { mstmt (While ($3, $6)) $loc }

| FOR LPAREN incomplete_body_statement ENDLINE expression ENDLINE incomplete_body_statement RPAREN LCURLY list(body_statement) RCURLY
  { mstmt (For ($3, $5, $7, $10)) $loc }

;

incomplete_body_statement: (* basically any one line statement *)
| ident ident ASSIGN_EQUALS expression
  { mstmt (Assign ($1, $2, $4)) $loc }

| incr
  { mstmt (Eval $1) $loc }

| func_call
  { mstmt (Eval $1) $loc }

| PRINT expression
  { mstmt (Print $2) $loc }

| PRINTLN expression
  { mstmt (PrintLn $2) $loc }
;


elseif:
| ELSE IF LPAREN expression RPAREN LCURLY list(body_statement) RCURLY
  { ($4, $7) }

| ELSE LCURLY list(body_statement) RCURLY
  { ((mexpr (Bool true) $loc), $3) }

(* one liner else ifs*)
| ELSE IF LPAREN expression RPAREN incomplete_body_statement ENDLINE
  { ($4, [$6]) }

| ELSE incomplete_body_statement ENDLINE
  { ((mexpr (Bool true) $loc), [$2]) }
;

param_def:
| ident ident
  { ($1, $2) }

ident:
| IDENT
  { midnt (Ident $1) $loc }
;

incr:
| INCREMENT expression
  { mexpr (PreIncr $2) $loc }

| expression INCREMENT
  { mexpr (PostIncr $1) $loc }
;

func_call:
| ident LPAREN separated_list(COMMA, expression) RPAREN
  { mexpr (FuncCall ($1, $3)) $loc }

expression:
(* litterals *)
| INT_LIT
  { mexpr (Int $1) $loc }

| MINUS INT_LIT
  { mexpr (Int (-$2)) $loc }

| BOOL_LIT
  { mexpr (Bool $1) $loc }

| STRING_LIT
  { mexpr (EString $1) $loc }

| SYSTIME
  { mexpr Systime $loc }

| ident
  { mexpr (Var $1) $loc }

| incr
  { $1 }

| func_call
  { $1 }

(* mathematical expressions *)
| expression PLUS expression (* { $1 + $3 } *)
  { mexpr (Plus ($1, $3)) $loc }

| expression MINUS expression (* { $1 - $3 } *)
  { mexpr (Minus ($1, $3)) $loc }

| expression TIMES expression (* { $1 * $3 } *)
  { mexpr (Times ($1, $3)) $loc }

| expression DIV expression (* { $1 / $3 } *)
  { mexpr (Div ($1, $3)) $loc }

| expression MODULO expression
  { mexpr (Modulo ($1, $3)) $loc }

| expression LESS_EQ expression
  { mexpr (LessEq ($1, $3)) $loc }

| expression GREATER_EQ expression
  { mexpr (GreaterEq ($1, $3)) $loc }

| expression LESS expression
  { mexpr (Less ($1, $3)) $loc }

| expression GREATER expression
  { mexpr (Greater ($1, $3)) $loc }

(* boolean expressions *)
| expression AND expression
  { mexpr (And ($1, $3)) $loc }

| expression OR expression
  { mexpr (Or ($1, $3)) $loc }

| expression BOOL_EQUALS expression
  { mexpr (Equals ($1, $3)) $loc }

| NOT expression
  { mexpr (Not $2) $loc }

| expression TERNARY_QUESTIONMARK expression TERNARY_COLON expression
  { mexpr (Ternary ($1, $3, $5)) $loc }

| LPAREN expression RPAREN
  { $2 }

;
