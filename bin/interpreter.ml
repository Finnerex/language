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

let rec flatten_list (accum:'b -> 'a -> unit) (l:'a list) i:'b =
  match l with
  | [] -> i
  | sm :: l2 -> accum i sm; flatten_list accum l2 i

let print_addr a = 
  let address = 2*(Obj.magic a) in
  Printf.printf "%d\n" address;;

let rec eval_statement (state:PrgmSt.t) (sm:statement) = 
  match sm with

  | Assign(v, e) -> 
    PrgmSt.add_var state v (eval_expr state e)
  
  | FuncDef(i, vl, sml) ->
    PrgmSt.add_func state i (vl, sml)
  
  | FuncCall(i, el) ->
    let (vl, sml) = PrgmSt.find_func state i in
    Printf.printf "addr before:";
    print_addr (match state with | PrgmSt(sl) -> sl);
    PrgmSt.push_stack state;
    Printf.printf "addr after push:";
    print_addr (match state with | PrgmSt(sl) -> sl);
    List.combine vl (List.map (eval_expr state) el) |> PrgmSt.add_vars state;
    List.iter (eval_statement state) sml;
    Printf.printf "addr after evaluation:";
    print_addr (match state with | PrgmSt(sl) -> sl);
    PrgmSt.pop_stack state;
    Printf.printf "addr after pop:";
    print_addr (match state with | PrgmSt(sl) -> sl);

  | If(l) ->
    (match l with
    | [] -> ()
    | (b, sml) :: xs ->
      (match eval_expr state b with
      | Bool true -> let _ = flatten_list eval_statement sml state in ()
      | Bool false -> eval_statement state (If xs); ()
      | _ -> raise TypeMismatch))
  
  | While(e, sml) ->
    (match eval_expr state e with
    | Bool true -> 
      let new_state = flatten_list eval_statement sml state in 
      eval_statement new_state sm

    | Bool false -> ()
    | _ -> raise TypeMismatch)

  | For(is, e, ls, sml) ->
    let _ = eval_statement state is in
    eval_statement state (While(e, sml @ [ls]))

  | Print(e) -> 
    Printf.printf "%s" (match eval_expr state e with
     | Int x -> string_of_int x
     | Bool x -> string_of_bool x
     | EString x -> x
     | _ -> "somethin else");
     ()

  | PrintLn(e) ->
    let state = eval_statement state (Print e) in
    Printf.printf "\n";
    state
  
  (* | _ -> raise Unimplemented *)

let eval_statements (sml:statement list) (state:PrgmSt.t) =
  flatten_list eval_statement sml state
