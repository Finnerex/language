(* ass syntax tree *)

(* Identifier *)
module Ident =
  struct
    type t =
    | Ident of string
    let compare x y = 
      match (x, y) with
      | Ident str1, Ident str2 ->
        compare str1 str2
  end

(* Expressions *)
type expr =
(* basic types *)
| Int of int
| Bool of bool
| EString of string

| Var of Ident.t

(* boolean expressions *)
| And of expr * expr
| Or of expr * expr
| Equals of expr * expr
| Not of expr

(* mathematical expressions *)
| Plus of expr * expr
| Minus of expr * expr
| Times of expr * expr
| Div of expr * expr

| Ternary of expr * expr * expr

(* Statements *)
type statement =
| Assign of Ident.t * expr
| Print of expr
| PrintLn of expr
