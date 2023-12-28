open Ast

exception Unimplemented
exception Type_mismatch of pos * pos * string
exception Unknown_type of string

module IdentMap = Map.Make(Ident)

module TypeChk = struct
  type t =
  | TypeChk of e_type IdentMap.t list
  let push tchk =
    match tchk with
    | TypeChk its -> TypeChk (IdentMap.empty :: its)
  let pop tchk =
    match tchk with
    | TypeChk (_ :: its) -> TypeChk (its)
    | TypeChk [] -> failwith "Invalid TypeChk"
  let add i t tchk = 
    match tchk with
    | TypeChk (itm :: its) -> TypeChk (IdentMap.add i.ident t itm :: its)
    | TypeChk [] -> failwith "Invalid TypeChk"
  let rec add_all its tchk =
    match its with
    | (i, t) :: pits -> 
      add_all pits tchk
      |> add i t
    | [] -> tchk
  let rec gamma i tchk =
    match tchk with
    | TypeChk (itm :: its) ->
      (match IdentMap.find_opt i.ident itm with
      | None -> gamma i (TypeChk its)
      | Some t -> Ok t)
    | TypeChk [] -> Error Not_found
  let empty () = TypeChk [IdentMap.empty]
end

let type_of_string s =
  match s with
  | "int" -> Ok TInt
  | "bool" -> Ok TBool
  | "string" -> Ok TString
  | "void" -> Ok TUnit
  | _ -> Error (Unknown_type s)

type expr_list = ExprList of expr list
[@@deriving show]

type e_type_list = ETypeList of e_type list
[@@deriving show]

let rec typecheck_expr (tchk:TypeChk.t) (e:expr_info) : (e_type, exn) result =
  match e.expr with
  | Int _ -> Ok TInt
  | Bool _ -> Ok TBool
  | EString _ -> Ok TString
  | Unit -> Ok TUnit
  | TypedExpr (t, _) -> Ok t
  | Var i -> TypeChk.gamma i tchk
  | Systime -> Ok TInt

  | FuncCall (i, pel) ->
    (match TypeChk.gamma i tchk with
    | Ok TFunc (r, ptl) ->
      check_pl tchk pel ptl
      |> Result.map (fun _ -> r)
    | Error e -> Error e
    | Ok t -> Error (Type_mismatch (e.pos_start, e.pos_end, string_of_ident i.ident ^ "was expected to be of type TFunc, instead " ^ show_e_type t)))
    
  | PreIncr e ->
    Result.bind (typecheck_expr tchk e) (fun t -> if t = TInt then Ok TInt else Error (Type_mismatch (e.pos_start, e.pos_end, "Expected TInt, instead found " ^ show_e_type t)))
  | PostIncr e ->
    Result.bind (typecheck_expr tchk e) (fun t -> if t = TInt then Ok TInt else Error (Type_mismatch (e.pos_start, e.pos_end, "Expected TInt, instead found " ^ show_e_type t)))
  
  | And (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TBool, Ok TBool -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TBool, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TBool, instead found " ^ show_e_type t))
    | Ok t, Ok TBool -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TBool, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TBool && TBool), instead found (" ^ show_e_type t1 ^ " && " ^ show_e_type t2 ^ ")")))
  | Or (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TBool, Ok TBool -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TBool, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TBool, instead found " ^ show_e_type t))
    | Ok t, Ok TBool -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TBool, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TBool || TBool), instead found (" ^ show_e_type t1 ^ " || " ^ show_e_type t2 ^ ")")))
  | Equals (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok _, Ok _ -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    (*| _ -> Error Mismatch_type *))
  | Not e ->
    (match typecheck_expr tchk e with
    | Ok TBool -> Ok TBool
    | Ok t -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected TBool, instead found " ^ show_e_type t))
    | Error err -> Error err)
  
  | Greater (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt > TInt), instead found (" ^ show_e_type t1 ^ " > " ^ show_e_type t2 ^ ")")))
  | GreaterEq (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt >= TInt), instead found (" ^ show_e_type t1 ^ " >= " ^ show_e_type t2 ^ ")")))
  | Less (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt < TInt), instead found (" ^ show_e_type t1 ^ " < " ^ show_e_type t2 ^ ")")))
  | LessEq (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt <= TInt), instead found (" ^ show_e_type t1 ^ " <= " ^ show_e_type t2 ^ ")")))
  
  | Plus (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt + TInt), instead found (" ^ show_e_type t1 ^ " + " ^ show_e_type t2 ^ ")")))
  | Minus (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt - TInt), instead found (" ^ show_e_type t1 ^ " - " ^ show_e_type t2 ^ ")")))
  | Times (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt * TInt), instead found (" ^ show_e_type t1 ^ " * " ^ show_e_type t2 ^ ")")))
  | Div (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt / TInt), instead found (" ^ show_e_type t1 ^ " / " ^ show_e_type t2 ^ ")")))
  | Modulo (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok TInt, Ok t -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t, Ok TInt -> Error (Type_mismatch (e2.pos_start, e2.pos_end, "Expected TInt, instead found " ^ show_e_type t))
    | Ok t1, Ok t2 -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected (TInt % TInt), instead found (" ^ show_e_type t1 ^ " % " ^ show_e_type t2 ^ ")")))
  
  | Ternary (e1, e2, e3) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    let t3 = typecheck_expr tchk e3 in
    (match t1, t2, t3 with
    | Ok TBool, Ok t2, Ok t3 ->
      if t2 = t3 then
        Ok t2
      else
        Error (Type_mismatch (e2.pos_start, e3.pos_end, "Expression types don't match"))
    | Error err, _,  _ -> Error err
    | _, Error err, _ -> Error err
    | _, _, Error err -> Error err
    | Ok t, _, _ -> Error (Type_mismatch (e1.pos_start, e1.pos_end, "Expected TBool, instead found " ^ show_e_type t)))

and check_exprs tchk el =
  match el with
  | [] -> Ok []
  | e :: next_el -> 
    let tr = typecheck_expr tchk e in
    (match tr with
    | Ok t -> check_exprs tchk next_el |> Result.map (fun tl -> t :: tl)
    | Error err -> Error err)

and check_pl tchk pel ptl =
  match pel, ptl with
  | pe :: ppel, pt :: pptl -> 
    (match check_pl tchk ppel pptl with
    | Ok () ->
      let t = typecheck_expr tchk pe in
      Result.bind t (fun t ->
        if t = pt then
          Ok ()
        else
          Error (Type_mismatch (pe.pos_start, pe.pos_end, "Type of passed argument doesn't match type of parameter")))
    | Error e -> Error e)
  | [], [] -> Ok ()
  | [], _ -> raise (Invalid_argument "list lengths don't match")
  | _, [] -> raise (Invalid_argument "list lengths don't match")

let rec resolve_type_results = function
  | [] -> Ok []
  | Ok r :: rts ->
    resolve_type_results rts
    |> Result.map (fun ts -> r :: ts)
  | Error err :: _ -> Error err

let string_of_ident (ti:Ident.t) =
  match ti with
  | Ident ts -> ts

let string_of_ident_info (ti:ident_info) =
  string_of_ident ti.ident

let create_function_type_and_params rts pl =
  let til, vil = List.split pl in
  let tlr =
    List.map string_of_ident_info til
    |> List.map type_of_string in
  match resolve_type_results tlr with
  | Ok tl ->
    type_of_string rts
    |> Result.map (fun t -> TFunc (t, tl))
    |> Result.map (fun ft -> ft, List.combine vil tl)
  | Error err -> Error err

let rec typecheck_statement (s:stmt_info) (tchk:TypeChk.t) (rt) : (TypeChk.t, exn) result =
  match s.stmt with
  | Assign (id, ni, e) ->
    let Ident vts = id.ident in
    let vtr = type_of_string vts in
    let etr = typecheck_expr tchk e in
    (match vtr, etr with
    | Ok vt, Ok et ->
      if vt = et then
        Ok (TypeChk.add ni et tchk)
      else
        Error (Type_mismatch (s.pos_start, s.pos_end, "Variable type and expression type don't match"))
    | Error err, _ -> Error err
    | _, Error err -> Error err)
  | Eval e ->
    (match typecheck_expr tchk e with
    | Ok _ -> Ok tchk
    | Error err -> Error err)
  | If l ->
    (match l with
    | [] -> Ok tchk
    | (e, sl) :: l ->
      (match typecheck_expr tchk e, check_statements tchk sl rt with
      | Ok TBool, Ok tchk -> typecheck_statement { stmt = (If l); pos_start = s.pos_start; pos_end = s.pos_end } tchk rt
      | Ok TBool, Error err -> Error err
      | Ok t, _ -> Error (Type_mismatch (e.pos_start, e.pos_end, "Expected TBool, instead found " ^ show_e_type t))
      | Error err, _ -> Error err))
  | While (e, sl) -> typecheck_statement ({ stmt = If [e, sl]; pos_start = s.pos_start; pos_end = s.pos_end }) tchk rt
  | For (a, e, is, sl) ->
    let chk_a = typecheck_statement a tchk rt in
    let new_tchk = Result.value chk_a ~default:tchk in
    let chk_w = typecheck_statement ({ stmt = While (e, sl @ [is]); pos_start = s.pos_start; pos_end = s.pos_end }) new_tchk rt in
    (match chk_a, chk_w with
    | Ok _, Ok tchk -> Ok tchk
    | Error err, _ -> Error err
    | _, Error err -> Error err)
  | FuncDef(rti, ni, pl, sl) ->
    let Ident rts = rti.ident in
    let ptchk = TypeChk.push tchk in
    let ft = create_function_type_and_params rts pl in
    (match ft with
    | Ok (TFunc (rt, ptl), pl) ->
      check_statements (TypeChk.add_all pl ptchk) sl rt
      |> Result.map (fun tchk -> TypeChk.pop tchk)
      |> Result.map (fun tchk -> TypeChk.add ni (TFunc (rt, ptl)) tchk)
    | Error err -> Error err
    | Ok _ -> failwith "create_function_type_object didn't return TFunc")
  | Return e ->
    (match typecheck_expr tchk e with
    | Ok t ->
      if t = rt then
        Ok tchk
      else
        Error (Type_mismatch (e.pos_start, e.pos_end, "Return type and expression type don't match"))
    | Error err -> Error err)
  
  | PrintLn e -> typecheck_statement ({ stmt = Print e; pos_start = s.pos_start; pos_end = s.pos_end }) tchk rt
  | Print e ->
    (match typecheck_expr tchk e with
    | Ok _ -> Ok tchk
    | Error err -> Error err)
  
  (* | _ -> failwith ("Unimplemented: " ^ show_statement s) *)

and check_statements tchk l rt : (TypeChk.t, exn) result =
  match l with
  | [] -> Ok tchk
  | t :: l ->
    (match typecheck_statement t tchk rt with
    | Ok tchk -> check_statements tchk l rt
    | Error err -> Error err)
