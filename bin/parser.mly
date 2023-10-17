%{
  open Ast
%}

(* Litterals *)
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <string> IDENT
(* %token <string> STRING_LIT
%token <char> CHAR_LIT
%token <float> FLOAT_LIT *)

(*%token TYPE*)

%token ASSIGN_EQUALS

%token LPAREN RPAREN
(* %token LSCOPE RSCOPE *)

(* Arithmetic operators *)
%token PLUS MINUS
%token TIMES DIV

(* Boolean opperators *)
%token AND OR NOT BOOL_EQUALS

%token TERNARY_QUESTIONMARK TERNARY_COLON

%token PRINT

%token ENDLINE
%token EOF

(* precedents, lower prec comes first *)
%left TERNARY_QUESTIONMARK TERNARY_COLON
%right OR 
%right AND
%left BOOL_EQUALS
%left PLUS MINUS
%left TIMES DIV
%right NOT


(* actual parsing *)

%start program
%type <Ast.statement list> program

%%

program:
| list(statement) EOF
  { $1 }
;

statement:
(*| ident ASSIGN_EQUALS expression
  { Assign ($1, $3) }*)
| PRINT expression ENDLINE
  { PrintStm $2 }

(*ident:
| IDENT
  { Ident $1 }*)

expression:
(* litterals *)
| INT_LIT
  { Int $1 }
| MINUS INT_LIT
  { Int (-$2) }

| BOOL_LIT
  { Bool $1 }

(* mathematical expressions *)
| expression PLUS expression (* { $1 + $3 } *)
  { Plus($1, $3) }

| expression MINUS expression (* { $1 - $3 } *)
  { Minus($1, $3) }

| expression TIMES expression (* { $1 * $3 } *)
  { Times($1, $3) }

| expression DIV expression (* { $1 / $3 } *)
  { Div($1, $3) }

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
