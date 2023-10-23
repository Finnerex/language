open Ast

exception Invalid_state

module IdentMap = Map.Make(Ident);;

module StLvl =
  struct
    type t = 
    | StLvl of (Ident.t list * statement list) IdentMap.t * expr IdentMap.t
    let add_var s i v =
      match s with
      | StLvl(fm, vm) -> StLvl (fm, IdentMap.add i v vm)
    let rec add_vars s (ivl:(Ident.t * expr) list) =
      match ivl with
      | [] -> s
      | (i, v)::ivl2 -> add_vars (add_var s i v) ivl2
    let add_func s i f =
      match s with
      | StLvl(fm, vm) -> StLvl (IdentMap.add i f fm, vm)
    let find_var_opt s i =
      match s with
      | StLvl(_, vm) -> IdentMap.find_opt i vm
    let find_func_opt s i =
      match s with
      | StLvl(fm, _) -> IdentMap.find_opt i fm
    let empty =
      StLvl (IdentMap.empty, IdentMap.empty)
  end

module PrgmSt =
  struct
    type t =
    | PrgmSt of StLvl.t list
    let push_stack = function
    | PrgmSt(sl) ->
      let new_sl = StLvl.empty::sl in
      PrgmSt new_sl
    let pop_stack = function
    | PrgmSt(sl) -> 
      (match sl with
      | [] -> raise Invalid_state
      | _::new_sl -> PrgmSt new_sl)
    let add_var p i v =
      match p with
      | PrgmSt(sl) ->
        (match sl with
        | [] -> raise Invalid_state
        | s::new_sl ->
          let new_s = StLvl.add_var s i v in
          PrgmSt (new_s::new_sl))
    let add_vars p (ivl:(Ident.t * expr) list) =
      match p with
      | PrgmSt(sl) ->
        (match sl with
        | [] -> raise Invalid_state
        | s::new_sl ->
          let new_s = StLvl.add_vars s ivl in
          PrgmSt (new_s::new_sl))
    let add_func p i f =
      match p with
      | PrgmSt(sl) ->
        (match sl with
        | [] -> raise Invalid_state
        | s::new_sl ->
          let new_s = StLvl.add_func s i f in
          PrgmSt (new_s::new_sl))
    let rec find_var p i =
      match p with
      | PrgmSt(sl) ->
        (match sl with
        | [] -> raise Not_found
        | s::new_sl ->
          (match StLvl.find_var_opt s i with
          | Some(v) -> v
          | None -> find_var (PrgmSt new_sl) i))
    let rec find_func p i =
      match p with
      | PrgmSt(sl) ->
        (match sl with
        | [] -> raise Not_found
        | s::new_sl ->
          (match StLvl.find_func_opt s i with
          | Some(f) -> f
          | None -> find_func (PrgmSt new_sl) i))
    let empty =
      PrgmSt [StLvl.empty]
  end