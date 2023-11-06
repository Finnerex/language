open Ast

exception Invalid_state

module IdentMap = Map.Make(Ident);;

module StLvl =
  struct
    type t = 
    | StLvl of (Ident.t, (Ident.t list * statement list)) Hashtbl.t * (Ident.t, expr) Hashtbl.t
    let add_var s i v =
      match s with
      | StLvl(_, vm) -> Hashtbl.replace vm i v
    let rec add_vars s (ivl:(Ident.t * expr) list) =
      match ivl with
      | [] -> ()
      | (i, v)::ivl2 -> add_var s i v; add_vars s ivl2
    let add_func s i f =
      match s with
      | StLvl(fm, _) -> Hashtbl.replace fm i f
    let find_var_opt s i =
      match s with
      | StLvl(_, vm) -> Hashtbl.find_opt vm i
    let find_func_opt s i =
      match s with
      | StLvl(fm, _) -> Hashtbl.find_opt fm i
    let empty =
      StLvl (Hashtbl.create 100, Hashtbl.create 100)
  end

module PrgmSt =
  struct
    type t =
    | PrgmSt of StLvl.t list ref
    let push_stack = function
    | PrgmSt(sl) ->
      sl := StLvl.empty::!sl;;
    let pop_stack = function
    | PrgmSt(sl) -> 
      (match !sl with
      | [] -> raise Invalid_state
      | _::new_sl -> sl := new_sl)
    let add_var p i v =
      match p with
      | PrgmSt(sl) ->
        (match !sl with
        | [] -> raise Invalid_state
        | s::_ ->
          StLvl.add_var s i v)
    let add_vars p (ivl:(Ident.t * expr) list) =
      match p with
      | PrgmSt(sl) ->
        (match !sl with
        | [] -> raise Invalid_state
        | s::_ ->
          StLvl.add_vars s ivl)
    let add_func p i f =
      match p with
      | PrgmSt(sl) ->
        (match !sl with
        | [] -> raise Invalid_state
        | s::_ ->
          StLvl.add_func s i f)
    let rec find_var p i =
      match p with
      | PrgmSt(sl) ->
        (match !sl with
        | [] -> raise Not_found
        | s::new_sl ->
          (match StLvl.find_var_opt s i with
          | Some(v) -> v
          | None -> find_var (PrgmSt (ref new_sl)) i))
    let rec find_func p i =
      match p with
      | PrgmSt(sl) ->
        (match !sl with
        | [] -> raise Not_found
        | s::new_sl ->
          (match StLvl.find_func_opt s i with
          | Some(f) -> f
          | None -> find_func (PrgmSt (ref new_sl)) i))
    let empty =
      PrgmSt (ref [StLvl.empty])
  end