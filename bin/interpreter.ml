open Ast

exception TypeMismatch
exception Unimplemented

module PrgmSt = Map.Make(String);;
let identifierMap:((ident, expr) Hashtbl.t) = Hashtbl.create 9

let rec eval_expr (e:expr) =
  match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | EString x -> EString x
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

  | Var(i) -> Hashtbl.find identifierMap i
  
  | _ -> raise Unimplemented

let rec eval_statement (state:expr PrgmSt.t) (sm:statement) = 
  match sm with

  | Assign(v, e) -> 
    (match v with 
    | Var Ident i -> PrgmSt.add i (eval_expr e) state
    | _ -> raise TypeMismatch)

  | Print(e) -> 
    Printf.printf "%s" (match eval_expr e with
     | Int x -> string_of_int x
     | Bool x -> string_of_bool x
     | EString x -> x
     | _ -> "somethin else");
     state

  | PrintLn(e) ->
    let state = eval_statement state (Print e) in
    Printf.printf "\n";
    state

let rec eval_statements (sml:statement list) (state:expr PrgmSt.t) =
  match sml with
  | [] -> ()
  | sm :: l -> eval_statement state sm |> eval_statements l
