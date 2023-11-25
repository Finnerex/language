(* ass syntax tree *)

(* Identifier *)
module Ident =
  struct
    type t =
    | Ident of string
    [@@deriving show]
    let compare x y = 
      match (x, y) with
      | Ident str1, Ident str2 ->
        compare str1 str2
  end

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

| Var of Ident.t
| TypedExpr of e_type * expr

| FuncCall of Ident.t * expr list

| PreIncr of expr
| PostIncr of expr

(* boolean expressions *)
| And of expr * expr
| Or of expr * expr
| Equals of expr * expr
| Not of expr

| Greater of expr * expr
| GreaterEq of expr * expr
| Less of expr * expr
| LessEq of expr * expr

(* mathematical expressions *)
| Plus of expr * expr
| Minus of expr * expr
| Times of expr * expr
| Div of expr * expr
| Modulo of expr * expr

| Ternary of expr * expr * expr
[@@deriving show]

(* Statements *)
type statement =
| Assign of Ident.t * Ident.t * expr
| Eval of expr

| If of (expr * statement list) list

| While of expr * statement list
| For of statement * expr * statement * statement list

| FuncDef of Ident.t * Ident.t * Ident.t list * statement list
| Return of expr

| Print of expr
| PrintLn of expr
[@@deriving show]
