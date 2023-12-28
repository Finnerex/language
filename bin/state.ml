open Ast

exception Invalid_state

module IdentMap = Map.Make(Ident);;

module StLvl =
  struct
    type t = 
    | StLvl of (Ident.t, (Ident.t list * stmt_info list)) Hashtbl.t * (Ident.t, (e_type * expr_info)) Hashtbl.t
    let add_var s i v t =
      match s with
      | StLvl(fm, vm) -> Hashtbl.replace vm i (t, v); StLvl (fm, vm)
    let rec add_vars s (ivl:(Ident.t * expr_info * e_type) list) =
      match ivl with
      | [] -> s
      | (i, v, t)::ivl2 -> add_vars (add_var s i v t) ivl2
    let add_func s i f =
      match s with
      | StLvl(fm, vm) -> Hashtbl.replace fm i f; StLvl (fm, vm)
    let find_var_opt s i =
      match s with
      | StLvl(_, vm) -> Hashtbl.find_opt vm i
    let find_func_opt s i =
      match s with
      | StLvl(fm, _) -> Hashtbl.find_opt fm i
    let empty () =
      StLvl (Hashtbl.create 64, Hashtbl.create 32)
  end

module PrgmSt =
  struct
    type t =
    | PrgmSt of StLvl.t list
    let push_stack = function
    | PrgmSt(sl) ->
      PrgmSt (StLvl.empty ()::sl)
    let pop_stack = function
    | PrgmSt(sl) -> 
      (match sl with
      | [] -> raise Invalid_state
      | _::new_sl -> PrgmSt new_sl)
    let rec try_replace_var p i v t =
      match p with
      | PrgmSt(sl) ->
        (match sl with
        | [] -> Error(Not_found)
        | s::new_sl -> 
          (match StLvl.find_var_opt s i with
          | None -> 
            let sl_result = try_replace_var (PrgmSt (new_sl)) i v t in
            (match sl_result with
            | Error(e) -> Error(e)
            | Ok(PrgmSt(found_sl)) -> Ok(PrgmSt(s::found_sl)))

          | Some(_) -> 
            let mod_s = StLvl.add_var s i v t in
            Ok(PrgmSt(mod_s::new_sl))))
    let add_var p i v t =
      match try_replace_var p i v t with
      | Ok(new_p) -> new_p
      | Error(_) ->
        (match p with
        | PrgmSt(sl) ->
          (match sl with
          | [] -> raise Invalid_state
          | s::new_sl ->
            let new_s = StLvl.add_var s i v t in
            PrgmSt (new_s::new_sl)))
    let add_vars p (ivl:(Ident.t * expr_info * e_type) list) =
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
        (match 
        sl with
        | [] -> raise Not_found
        | s::new_sl ->
          (match StLvl.find_func_opt s i with
          | Some(f) -> f
          | None -> find_func (PrgmSt new_sl) i))
    let empty =
      PrgmSt [StLvl.empty ()]
  end
