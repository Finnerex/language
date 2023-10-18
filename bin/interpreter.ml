open Ast

exception TypeMismatch
exception Unimplemented

[@@@ocaml.warning "-27"]

module PrgmSt = Map.Make(Ident);;

let rec eval_expr (state:expr PrgmSt.t) (e:expr) =
  match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | EString x -> EString x
  | Plus(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Int(i1 + i2)
    | _ -> raise TypeMismatch)
    
  | Minus(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Int(i1 - i2)
    | _ -> raise TypeMismatch)
    
  | Times(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Int(i1 * i2)
    | _ -> raise TypeMismatch)
    
  | Div(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Int(i1 / i2)
    | _ -> raise TypeMismatch)

  | And(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Bool b1, Bool b2 -> Bool(b1 && b2)
    | _ -> raise TypeMismatch)

  | Or(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Bool b1, Bool b2 -> Bool(b1 || b2)
    | _ -> raise TypeMismatch)

  | Equals(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Bool(i1 = i2)
    | Bool b1, Bool b2 -> Bool(b1 = b2)
    | _ -> raise TypeMismatch)

  | Ternary(c, e1, e2) ->
    (match eval_expr state c with
    | Bool true -> eval_expr state e1
    | Bool false -> eval_expr state e2
    | _ -> raise TypeMismatch)

  | Var(i) -> PrgmSt.find i state
  
  | _ -> raise Unimplemented

let rec eval_statement (state:expr PrgmSt.t) (sm:statement) = 
  match sm with

  | Assign(v, e) -> 
    PrgmSt.add v (eval_expr state e) state

  | If(l) ->
    (match l with
    | [] -> state
    | (b, sml) :: xs ->
      (match eval_expr state b with
      | Bool true -> (* eval_statements sml *) state
      | Bool false -> eval_statement state (If xs)
      | _ -> raise TypeMismatch))

  | Print(e) -> 
    Printf.printf "%s" (match eval_expr state e with
     | Int x -> string_of_int x
     | Bool x -> string_of_bool x
     | EString x -> x
     | _ -> "somethin else");
     state

  | PrintLn(e) ->
    let state = eval_statement state (Print e) in
    Printf.printf "\n";
    state
  
  (* | _ -> raise Unimplemented *)

let rec eval_statements (sml:statement list) (state:expr PrgmSt.t) =
  match sml with
  | [] -> ()
  | sm :: l -> eval_statement state sm |> eval_statements l
