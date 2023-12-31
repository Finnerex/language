open Ast
open State

exception Type_mismatch of string
exception Unimplemented

(* [@@@ocaml.warning "-27"] *)

(** @param e expected
    @param f found
    @raise [Type_mismatch]*)
let mismatch_type e f : 'a =
  raise (Type_mismatch ("Found [" ^ f ^ "], expected [" ^ e ^ "]"))

let rec flatten_list (accum:'b -> 'a -> 'b) (l:'a list) (i:'b) =
  match l with
  | [] -> i
  | sm :: l2 -> accum i sm |> flatten_list accum l2

let rec combine3 sa sb sc =
  match sa, sb, sc with
  | [], [], [] -> []
  | a::sa, b::sb, c::sc -> (a, b, c)::(combine3 sa sb sc)
  | _ -> raise (Invalid_argument "different lengths")

let rec eval_expr (state:PrgmSt.t) (e:expr_info) =
  match e.expr with
  | Int(x) -> Int(x), state
  | Bool(x) -> Bool(x), state
  | EString(x) -> EString(x), state
  | Unit -> Unit, state

  | Systime -> (Int (int_of_float (Sys.time () *. 1000.0)), state)

  | FuncCall(i, el) ->
    let (vl, sml) = PrgmSt.find_func state i.ident in
    let pushed_state = PrgmSt.push_stack state in
    let new_state = List.combine vl (map_list_expr el pushed_state) |> PrgmSt.add_vars pushed_state in
    let ret, pc_state = eval_list_and_return sml new_state in
    PrgmSt.pop_stack pc_state |> fun s -> ret, s

  | Plus(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 + i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
  
  | Minus(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 - i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
    
  | Times(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 * i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
    
  | Div(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 / i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
  
  | PreIncr(v) ->
    (match v.expr with
    | Var(id) ->
      let e, state2 = eval_expr state v in
      (match e with
      | Int i ->
        let iv = Int (i + 1) in
        let iv_info = { expr = iv; pos_start = v.pos_start; pos_end = v.pos_end } in
        let state3 = PrgmSt.add_var state2 id.ident iv_info in
        (iv, state3)
      | _ -> raise (Invalid_argument (show_expr e)))
    | _ -> mismatch_type "Ast.Var" (show_expr v.expr))
  
  | PostIncr(v) ->
    (match v.expr with
    | Var(id) -> 
      let e, state2 = eval_expr state v in
      (match e with
      | Int i ->
        let new_v = Int (i + 1) in
        let new_v_info = { expr = new_v; pos_start = v.pos_start; pos_end = v.pos_end } in
        let state3 = PrgmSt.add_var state2 id.ident new_v_info in
        (e, state3)
      | _ -> raise (Invalid_argument (show_expr e)))
    | _ -> mismatch_type "Ast.Var" (show_expr v.expr))

  | Modulo(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Int(i1 mod i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
  
  | Less(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 < i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
  
  | LessEq(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 <= i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
  
  | Greater(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 > i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))
  
  | GreaterEq(e1, e2) ->
    let i1, state2 = eval_expr state e1 in
    let i2, state3 = eval_expr state2 e2 in
    (match i1, i2 with
    | Int i1, Int i2 -> (Bool(i1 >= i2), state3)
    | _ -> mismatch_type "Ast.Int, Ast.Int" (show_expr i1 ^ ", " ^ show_expr i2))

  | Not(x) ->
    let new_x, new_state = eval_expr state x in
    (match new_x with
    | Bool b -> (Bool (not b), new_state)
    | _ -> mismatch_type "Ast.Bool" (show_expr new_x))

  | And(e1, e2) ->
    let b1, state2 = eval_expr state e1 in
    let b2, state3 = eval_expr state2 e2 in
    (match b1, b2 with
    | Bool b1, Bool b2 -> (Bool(b1 && b2), state3)
    | _ -> mismatch_type "Ast.Bool, Ast.Bool" (show_expr b1 ^ ", " ^ show_expr b2))

  | Or(e1, e2) ->
    let b1, state2 = eval_expr state e1 in
    let b2, state3 = eval_expr state2 e2 in
    (match b1, b2 with
    | Bool b1, Bool b2 -> (Bool(b1 || b2), state3)
    | _ -> mismatch_type "Ast.Bool, Ast.Bool" (show_expr b1 ^ ", " ^ show_expr b2))

  | Equals(e1, e2) ->
    let x1, state2 = eval_expr state e1 in
    let x2, state3 = eval_expr state2 e2 in
    (match x1, x2 with
    | Int i1, Int i2 -> (Bool(i1 = i2), state3)
    | Bool b1, Bool b2 -> (Bool(b1 = b2), state3)
    | _ -> mismatch_type "(Ast.Bool, Ast.Bool)|(Ast.Int, Ast.Int)" (show_expr x1 ^ ", " ^ show_expr x2))

  | Ternary(c, e1, e2) ->
    let new_c, new_state = eval_expr state c in
    (match new_c with
    | Bool true -> eval_expr new_state e1
    | Bool false -> eval_expr new_state e2
    | _ -> mismatch_type "Ast.Bool" (show_expr new_c))

  | Var(i) -> (PrgmSt.find_var state i.ident).expr, state
  
  (* | _ -> raise Unimplemented *)

and eval_statement (state:PrgmSt.t) (sm:stmt_info) = 
  match sm.stmt with

  | Assign(id, v, e) ->
    let t =
    (match string_of_ident id.ident with
    | "int" -> TInt
    | "bool" -> TBool
    | "string" -> TString
    | _ -> raise Unimplemented) in
    let new_e, new_state = eval_expr state e in
    let new_e_info = { expr = new_e; pos_start = e.pos_start; pos_end = e.pos_end } in
    let et = eval_type new_state new_e_info in
    if t = et then
      PrgmSt.add_var new_state v.ident new_e_info
    else (mismatch_type (show_e_type t) (show_e_type et))
  
  | Eval(e) ->
    let _, new_state = eval_expr state e in
    new_state
  
  | FuncDef(_, i, pl, sml) ->
    let _, vl = List.split pl in
    PrgmSt.add_func state i.ident (List.map (fun ii -> ii.ident) vl, sml)

  | If(l) ->
    (match l with
    | [] -> state
    | (b, sml) :: xs ->
      let new_b, new_state = eval_expr state b in
      let pushed_state = PrgmSt.push_stack new_state in
      let if_state = (match new_b with
      | Bool true -> flatten_list eval_statement sml pushed_state
      | Bool false -> let new_if = If xs in
        eval_statement pushed_state { stmt = new_if; pos_start = sm.pos_start; pos_end = sm.pos_end }
      | _ -> mismatch_type "Ast.Bool" (show_expr new_b)) in
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
    | _ -> mismatch_type "Ast.Bool" (show_expr new_e))

  | For(is, e, ls, sml) ->
    let pushed_state = PrgmSt.push_stack state in
    let new_state = eval_statement pushed_state is in
    let new_sm = While(e, sml @ [ls]) in
    let new_sm_info = { stmt = new_sm; pos_start = sm.pos_start; pos_end = sm.pos_end } in
    eval_statement new_state new_sm_info |> PrgmSt.pop_stack

  | Print(e) ->
    let new_e, new_state = eval_expr state e in
    Printf.printf "%s" (match new_e with
    | Int x -> string_of_int x
    | Bool x -> string_of_bool x
    | EString x -> x
    | _ -> "wtf: " ^ show_expr new_e);
    new_state

  | PrintLn(e) ->
    let new_sm = Print e in
    let new_sm_info = { stmt = new_sm; pos_start = sm.pos_start; pos_end = sm.pos_end } in
    let state = eval_statement state new_sm_info in
    Printf.printf "\n";
    state
  
  | _ -> raise Unimplemented

and eval_list_and_return (sl:stmt_info list) (state:PrgmSt.t) : (expr * PrgmSt.t) =
  match sl with
  | [] -> Unit, state
  | s::next_sl -> 
    (match s.stmt with
    | Return e -> eval_expr state e
    | _ -> eval_statement state s |> eval_list_and_return next_sl)

and map_list_expr el s =
  match el with
  | [] -> []
  | e :: new_el -> 
    let ce, new_s = eval_expr s e in
    let ce_info = { expr = ce; pos_start = e.pos_start; pos_end = e.pos_end } in
    ce_info :: map_list_expr new_el new_s

and eval_type (state:PrgmSt.t) (e:expr_info) =
  let new_e, _ = eval_expr state e in
  match new_e with
  | Int(_) -> TInt
  | Bool(_) -> TBool
  | EString(_) -> TString
  | Unit -> TUnit
  | _ -> raise (Invalid_argument ("Unable to evaluate type of " ^ show_expr new_e))

let eval_statements (sml:stmt_info list) (state:PrgmSt.t) =
  flatten_list eval_statement sml state
