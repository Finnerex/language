open Ast
open State

exception TypeMismatch
exception Unimplemented

(* [@@@ocaml.warning "-27"] *)

let rec eval_expr (state:PrgmSt.t) (e:expr) =
  match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | EString x -> EString x
  | Systime -> Int (int_of_float (Sys.time () *. 1000.0))
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
  | Modulo(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Int(i1 mod i2)
    | _ -> raise TypeMismatch)
  
  | Less(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Bool(i1 < i2)
    | _ -> raise TypeMismatch)
  
  | LessEq(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Bool(i1 <= i2)
    | _ -> raise TypeMismatch)
  
  | Greater(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Bool(i1 > i2)
    | _ -> raise TypeMismatch)
  
  | GreaterEq(e1, e2) ->
    (match eval_expr state e1, eval_expr state e2 with
    | Int i1, Int i2 -> Bool(i1 >= i2)
    | _ -> raise TypeMismatch)

  | Not(x) ->
    (match eval_expr state x with
    | Bool b -> Bool (not b)
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

  | Var(i) -> PrgmSt.find_var state i
  
  (* | _ -> raise Unimplemented *)

let rec flatten_list (accum:'b -> 'a -> 'b) (l:'a list) (i:'b) =
  match l with
  | [] -> i
  | sm :: l2 -> accum i sm |> flatten_list accum l2

let rec eval_statement (state:PrgmSt.t) (sm:statement) = 
  match sm with

  | Assign(v, e) -> 
    PrgmSt.add_var state v (eval_expr state e)
  
  | FuncDef(i, vl, sml) ->
    PrgmSt.add_func state i (vl, sml)
  
  | FuncCall(i, el) ->
    let (vl, sml) = PrgmSt.find_func state i in
    let pushed_state = PrgmSt.push_stack state in
    let new_state = List.combine vl (List.map (eval_expr pushed_state) el) |> PrgmSt.add_vars pushed_state in
    flatten_list eval_statement sml new_state |> PrgmSt.pop_stack

  | If(l) ->
    (match l with
    | [] -> state
    | (b, sml) :: xs ->
      (match eval_expr state b with
      | Bool true -> flatten_list eval_statement sml state
      | Bool false -> eval_statement state (If xs)
      | _ -> raise TypeMismatch))
  
  | While(e, sml) ->
    (match eval_expr state e with
    | Bool true -> 
      let new_state = flatten_list eval_statement sml state in 
      eval_statement new_state sm

    | Bool false -> state
    | _ -> raise TypeMismatch)

  | For(is, e, ls, sml) ->
    let new_state = eval_statement state is in
    eval_statement new_state (While(e, sml @ [ls]))

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

let eval_statements (sml:statement list) (state:PrgmSt.t) =
  flatten_list eval_statement sml state
