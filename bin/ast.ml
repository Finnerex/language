(* ass syntax tree *)

type pos = {
  line_num : int;
  char_num : int;
}
[@@deriving show]

(* Identifier *)
module Ident = struct
  type t = Ident of string [@@deriving show]

  let compare x y =
    match (x, y) with Ident str1, Ident str2 -> compare str1 str2
end

type ident_info = {
  ident : Ident.t;
  pos_start : pos;
  pos_end : pos;
}
[@@deriving show]

type e_type =
| TInt
| TBool
| TString
| TUnit
| TFunc of e_type * e_type list
[@@deriving show]
(* | Complex of e_type list *)

(* Expressions *)
type expr =
| Int of int
| Bool of bool
| EString of string
| Unit

| Systime

| Var of ident_info
| TypedExpr of e_type * expr_info

| FuncCall of ident_info * expr_info list

| PreIncr of expr_info
| PostIncr of expr_info

(* boolean expressions *)
| And of expr_info * expr_info
| Or of expr_info * expr_info
| Equals of expr_info * expr_info
| Not of expr_info

| Greater of expr_info * expr_info
| GreaterEq of expr_info * expr_info
| Less of expr_info * expr_info
| LessEq of expr_info * expr_info

(* mathematical expressions *)
| Plus of expr_info * expr_info
| Minus of expr_info * expr_info
| Times of expr_info * expr_info
| Div of expr_info * expr_info
| Modulo of expr_info * expr_info

| Ternary of expr_info * expr_info * expr_info
[@@deriving show]

and expr_info = {
  expr : expr;
  pos_start : pos;
  pos_end : pos;
}

(* Statements *)
type statement =
| Assign of ident_info * ident_info * expr_info
| Eval of expr_info

| If of (expr_info * stmt_info list) list

| While of expr_info * stmt_info list
| For of stmt_info * expr_info * stmt_info * stmt_info list

| FuncDef of ident_info * ident_info * (ident_info * ident_info) list * stmt_info list
| Return of expr_info

| Print of expr_info
| PrintLn of expr_info
[@@deriving show]

and stmt_info = {
  stmt : statement;
  pos_start : pos;
  pos_end : pos;
}
