%{
  open Ast
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
%type <Ast.statement list> program

%%

program:
| list(statement) EOF
  { $1 }
;


statement:
| incomplete_statement ENDLINE
  { $1 }

| RETURN expression ENDLINE
  { Return $2 }

| RETURN ENDLINE
  { Return Unit }
  
| IF LPAREN expression RPAREN LCURLY list(statement) RCURLY list(elseif)
  { If (($3, $6) :: $8) }

| IF LPAREN expression RPAREN incomplete_statement ENDLINE list(elseif) (* can only have one one-line statement *)
  { If (($3, [$5]) :: $7) }

| WHILE LPAREN expression RPAREN LCURLY list(statement) RCURLY
  { While ($3, $6) }

| FOR LPAREN incomplete_statement ENDLINE expression ENDLINE incomplete_statement RPAREN LCURLY list(statement) RCURLY
  { For ($3, $5, $7, $10) }

| ident ident LPAREN separated_list(COMMA, ident) RPAREN LCURLY list(statement) RCURLY
  { FuncDef ($1, $2, $4, $7) }

;

incomplete_statement: (* basically any one line statement *)
| ident ident ASSIGN_EQUALS expression
  { Assign ($1, $2, $4) }

| incr
  { Eval $1 }

| func_call
  { Eval $1 }

| PRINT expression
  { Print $2 } 

| PRINTLN expression
  { PrintLn $2 }
;


elseif:
| ELSE IF LPAREN expression RPAREN LCURLY list(statement) RCURLY
  { ($4, $7) }

| ELSE LCURLY list(statement) RCURLY
  { ((Bool true), $3) }

(* one liner else ifs*)
| ELSE IF LPAREN expression RPAREN incomplete_statement ENDLINE
  { ($4, [$6]) }

| ELSE incomplete_statement ENDLINE
  { ((Bool true), [$2]) }
;


ident:
| IDENT
  { Ident $1 }
;

incr:
| INCREMENT expression
  { PreIncr $2 }

| expression INCREMENT
  { PostIncr $1 }
;

func_call:
| ident LPAREN separated_list(COMMA, expression) RPAREN
  { FuncCall($1, $3) }

expression:
(* litterals *)
| INT_LIT
  { Int $1 }

| MINUS INT_LIT
  { Int (-$2) }

| BOOL_LIT
  { Bool $1 }

| STRING_LIT
  { EString $1 }

| SYSTIME
  { Systime }

| ident
  { Var $1 }

| incr
  { $1 }

| func_call
  { $1 }

(* mathematical expressions *)
| expression PLUS expression (* { $1 + $3 } *)
  { Plus($1, $3) }

| expression MINUS expression (* { $1 - $3 } *)
  { Minus($1, $3) }

| expression TIMES expression (* { $1 * $3 } *)
  { Times($1, $3) }

| expression DIV expression (* { $1 / $3 } *)
  { Div($1, $3) }

| expression MODULO expression
  { Modulo($1, $3) }

| expression LESS_EQ expression
  { LessEq($1, $3) }

| expression GREATER_EQ expression
  { GreaterEq($1, $3) }

| expression LESS expression
  { Less($1, $3) }

| expression GREATER expression
  { Greater($1, $3) }

(* boolean expressions *)
| expression AND expression
  { And($1, $3) }

| expression OR expression
  { Or($1, $3) }

| expression BOOL_EQUALS expression
  { Equals($1, $3) }

| NOT expression
  { Not($2) }

| expression TERNARY_QUESTIONMARK expression TERNARY_COLON expression
  { Ternary($1, $3, $5) }

| LPAREN expression RPAREN
  { $2 }

;
