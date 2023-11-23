open Ast
open State

exception TypeMismatch
exception Unimplemented

(* [@@@ocaml.warning "-27"] *)



let rec eval_expr (state:PrgmSt.t) (e:expr) =
  match e with
  | Int(x) -> Int(x), state
  | Bool(x) -> Bool(x), state
  | EString(x) -> EString(x), state

  | Systime -> (Int (int_of_float (Sys.time () *. 1000.0)), state)

  | Plus(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 + i2), state3)
    | _ -> raise TypeMismatch)
  
  | Minus(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 - i2), state3)
    | _ -> raise TypeMismatch)
    
  | Times(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 * i2), state3)
    | _ -> raise TypeMismatch)
    
  | Div(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 / i2), state3)
    | _ -> raise TypeMismatch)
  
  | PreIncr(v) ->
    (match v with
    | Var(id) ->
      let e, state2 = eval_expr state v in
      (match e with
      | TypedExpr (t, Int i) ->
        let iv = Int (i + 1) in
        let state3 = PrgmSt.add_var state2 id iv t in
        (iv, state3)
      | _ -> raise TypeMismatch)
    | _ -> raise TypeMismatch)
  
  | PostIncr(v) ->
    (match v with
    | Var(id) -> 
      let e, state2 = eval_expr state v in
      (match e with
      | TypedExpr (t, Int i) ->
        let state3 = PrgmSt.add_var state2 id (Int (i + 1)) t in
        (e, state3)
      | _ -> raise TypeMismatch)
    | _ -> raise TypeMismatch)

  | Modulo(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 mod i2), state3)
    | _ -> raise TypeMismatch)
  
  | Less(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 < i2), state3)
    | _ -> raise TypeMismatch)
  
  | LessEq(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 <= i2), state3)
    | _ -> raise TypeMismatch)
  
  | Greater(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 > i2), state3)
    | _ -> raise TypeMismatch)
  
  | GreaterEq(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 >= i2), state3)
    | _ -> raise TypeMismatch)

  | Not(x) ->
    let new_x, new_state = eval_expr state x in
    (match new_x with
    | Bool b -> (Bool (not b), new_state)
    | _ -> raise TypeMismatch)

  | And(e1, e2) ->
    let b1, state2 = eval_expr state e1 in
    let b2, state3 = eval_expr state2 e2 in
    (match b1, b2 with
    | Bool b1, Bool b2 -> (Bool(b1 && b2), state3)
    | _ -> raise TypeMismatch)

  | Or(e1, e2) ->
    let b1, state2 = eval_expr state e1 in
    let b2, state3 = eval_expr state2 e2 in
    (match b1, b2 with
    | Bool b1, Bool b2 -> (Bool(b1 || b2), state3)
    | _ -> raise TypeMismatch)

  | Equals(e1, e2) ->
    let x1, state2 = eval_expr state e1 in
    let x2, state3 = eval_expr state2 e2 in
    (match x1, x2 with
    | Int i1, Int i2 -> (Bool(i1 = i2), state3)
    | Bool b1, Bool b2 -> (Bool(b1 = b2), state3)
    | _ -> raise TypeMismatch)

  | Ternary(c, e1, e2) ->
    let new_c, new_state = eval_expr state c in
    (match new_c with
    | Bool true -> eval_expr new_state e1
    | Bool false -> eval_expr new_state e2
    | _ -> raise TypeMismatch)

  | Var(i) -> let t, e = PrgmSt.find_var state i in TypedExpr(t, e), state

  | TypedExpr(t, e) -> TypedExpr(t, e), state
  
  (* | _ -> raise Unimplemented *)

  let eval_type (state:PrgmSt.t) (e:expr) =
    let new_e, _ = eval_expr state e in
    match new_e with
    | TypedExpr(t, _) -> t
    | Int(_) -> TInt
    | Bool(_) -> TBool
    | EString(_) -> TString
    | _ -> raise TypeMismatch

let rec flatten_list (accum:'b -> 'a -> 'b) (l:'a list) (i:'b) =
  match l with
  | [] -> i
  | sm :: l2 -> accum i sm |> flatten_list accum l2

let rec map_list_expr el s =
  match el with
  | [] -> []
  | e :: new_el -> let ce, new_s = eval_expr s e in
    ce :: map_list_expr new_el new_s

let rec eval_statement (state:PrgmSt.t) (sm:statement) = 
  match sm with

  | Assign(Ident(s), v, e) ->
    let t =
    (match s with
    | "int" -> TInt
    | "bool" -> TBool
    | "string" -> TString
    | _ -> raise Unimplemented) in
    let new_e, new_state = eval_expr state e in
    PrgmSt.add_var new_state v new_e t
  
  | Eval(e) ->
    let _, new_state = eval_expr state e in
    new_state
  
  | FuncDef(i, vl, sml) ->
    PrgmSt.add_func state i (vl, sml)
  
  | FuncCall(i, el) ->
    let (vl, sml) = PrgmSt.find_func state i in
    let pushed_state = PrgmSt.push_stack state in
    let new_state = List.combine vl (map_list_expr el pushed_state) |> PrgmSt.add_vars pushed_state in
    flatten_list eval_statement sml new_state |> PrgmSt.pop_stack

  | If(l) ->
    (match l with
    | [] -> state
    | (b, sml) :: xs ->
      let new_b, new_state = eval_expr state b in
      let pushed_state = PrgmSt.push_stack new_state in
      let if_state = (match new_b with
      | Bool true -> flatten_list eval_statement sml pushed_state
      | Bool false -> eval_statement pushed_state (If xs)
      | _ -> raise TypeMismatch) in
      PrgmSt.pop_stack if_state)
  
  | While(e, sml) ->
    let new_e, new_state = eval_expr state e in
    (match new_e with
    | Bool true ->
      let pushed_state = PrgmSt.push_stack new_state in
      let eval_state = flatten_list eval_statement sml pushed_state in
      let popped_state = PrgmSt.pop_stack eval_state in
      eval_statement popped_state sm

    | Bool false -> new_state
    | _ -> raise TypeMismatch)

  | For(is, e, ls, sml) ->
    let pushed_state = PrgmSt.push_stack state in
    let new_state = eval_statement pushed_state is in
    eval_statement new_state (While(e, sml @ [ls])) |> PrgmSt.pop_stack

  | Print(e) -> 
    let new_e, new_state = eval_expr state e in
    Printf.printf "%s" (match new_e with
     | Int x -> string_of_int x
     | Bool x -> string_of_bool x
     | EString x -> x
     | _ -> "somethin else");
     new_state

  | PrintLn(e) ->
    let state = eval_statement state (Print e) in
    Printf.printf "\n";
    state
  
  (* | _ -> raise Unimplemented *)

let eval_statements (sml:statement list) (state:PrgmSt.t) =
  flatten_list eval_statement sml state