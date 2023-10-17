open Ast

exception TypeMismatch
exception Unimplemented

let rec eval_expr (e:expr) =
  match e with
  | Int x -> Int x
  | Bool x -> Bool x
  
  | Plus(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Int i1, Int i2 -> Int(i1 + i2)
    | _ -> raise TypeMismatch)
    
  | Minus(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Int i1, Int i2 -> Int(i1 - i2)
    | _ -> raise TypeMismatch)
    
  | Times(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Int i1, Int i2 -> Int(i1 * i2)
    | _ -> raise TypeMismatch)
    
  | Div(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Int i1, Int i2 -> Int(i1 / i2)
    | _ -> raise TypeMismatch)

  | And(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Bool b1, Bool b2 -> Bool(b1 && b2)
    | _ -> raise TypeMismatch)

  | Or(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Bool b1, Bool b2 -> Bool(b1 || b2)
    | _ -> raise TypeMismatch)

  | Equals(e1, e2) ->
    (match eval_expr e1, eval_expr e2 with
    | Int i1, Int i2 -> Bool(i1 = i2)
    | Bool b1, Bool b2 -> Bool(b1 = b2)
    | _ -> raise TypeMismatch)

  | Ternary(c, e1, e2) ->
    (match eval_expr c with
    | Bool true -> eval_expr e1
    | Bool false -> eval_expr e2
    | _ -> raise TypeMismatch)

  
  
  | _ -> raise Unimplemented
