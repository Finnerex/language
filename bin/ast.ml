(* ass syntax tree *)

(* Identifier *)
type ident = Ident of string

(* Expressions *)
type expr =
(* basic types *)
| Int of int
| Bool of bool

| Var of ident

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
| Assign of ident * expr
| Print of expr
