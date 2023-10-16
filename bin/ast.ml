(* ass syntax tree *)

type expr =
(* basic types *)
| Int of int
| Bool of bool

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

(* type statement =
| If of expression * (statement list) *)
