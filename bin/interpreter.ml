open Ast

exception TypeMismatch
exception Unimplemented

(* [@@@ocaml.warning "-27"] *)

module IdentMap = Map.Make(Ident);;

module PrgmSt =
  struct
    type t = 
    | PrgmSt of (Ident.t list * statement list) IdentMap.t * expr IdentMap.t
    let add_var p i v =
      match p with
      | PrgmSt(fm, vm) -> PrgmSt (fm, IdentMap.add i v vm)
    let rec add_vars p (ivl:(Ident.t * expr) list) =
      match ivl with
      | [] -> p
      | (i, v)::ivl2 -> add_vars (add_var p i v) ivl2
    let add_func p i f =
      match p with
      | PrgmSt(fm, vm) -> PrgmSt (IdentMap.add i f fm, vm)
    let find_var p i =
      match p with
      | PrgmSt(_, vm) -> IdentMap.find i vm
    let find_func p i =
      match p with
      | PrgmSt(fm, _) -> IdentMap.find i fm
    let empty =
      PrgmSt (IdentMap.empty, IdentMap.empty)
  end

let rec eval_expr (state:PrgmSt.t) (e:expr) =
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
    let new_state = List.combine vl (List.map (eval_expr state) el) |> PrgmSt.add_vars state in
    flatten_list eval_statement sml new_state

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
