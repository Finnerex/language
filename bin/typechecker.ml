open Ast

exception Unimplemented
exception Type_mismatch
exception Unknown_type of string

module TypeChk = struct
  type t =
  | TypeChk of (Ident.t * e_type) list
  let add i t tchk = 
    match tchk with
    | TypeChk its -> TypeChk ((i, t)::its)
  let rec gamma i tchk =
    match tchk with
    | TypeChk its ->
      (match its with
      | [] -> raise Not_found
      | it::its ->
        let fi, ft = it in
        if fi = i then
          ft
        else
          gamma i (TypeChk its))
  let empty () = TypeChk []
end

let type_of_string s =
  match s with
  | "int" -> Ok TInt
  | "bool" -> Ok TBool
  | "string" -> Ok TString
  | "void" -> Ok TUnit
  | _ -> Error (Unknown_type s)

let rec typecheck_expr (tchk:TypeChk.t) (e:expr) : (e_type, exn) result =
  match e with
  | Int _ -> Ok TInt
  | Bool _ -> Ok TBool
  | EString _ -> Ok TString
  | Unit -> Ok TUnit
  | TypedExpr (t, _) -> Ok t
  | Var i -> Ok (TypeChk.gamma i tchk)
  | Systime -> Ok TInt

  | FuncCall (i, _) -> Ok (TypeChk.gamma i tchk)

  | PreIncr e ->
    Result.bind (typecheck_expr tchk e) (fun t -> if t = TInt then Ok TInt else Error Type_mismatch)
  | PostIncr e ->
    Result.bind (typecheck_expr tchk e) (fun t -> if t = TInt then Ok TInt else Error Type_mismatch)
  
  | And (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TBool, Ok TBool -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | Or (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TBool, Ok TBool -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
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
    | Ok _ -> Error Type_mismatch
    | Error err -> Error err)
  
  | Greater (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | GreaterEq (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | Less (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | LessEq (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  
  | Plus (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | Minus (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | Times (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | Div (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  | Modulo (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Type_mismatch)
  
  | Ternary (e1, e2, e3) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    let t3 = typecheck_expr tchk e3 in
    (match t1, t2, t3 with
    | Ok TBool, Ok t2, Ok t3 ->
      if t2 = t3 then
        Ok t2
      else
        Error Type_mismatch
    | Error err, _,  _ -> Error err
    | _, Error err, _ -> Error err
    | _, _, Error err -> Error err
    | _ -> Error Type_mismatch)

and check_exprs tchk el =
  match el with
  | [] -> Ok []
  | e :: next_el -> 
    let tr = typecheck_expr tchk e in
    (match tr with
    | Ok t -> check_exprs tchk next_el |> Result.map (fun tl -> t :: tl)
    | Error err -> Error err)

let rec resolve_type_results = function
  | [] -> Ok []
  | Ok r :: rts ->
    resolve_type_results rts
    |> Result.map (fun ts -> r :: ts)
  | Error err :: _ -> Error err

let string_of_ident (ti:Ident.t) =
  match ti with
  | Ident ts -> ts

let create_function_type_object rts pl =
  let til, _ = List.split pl in
  let tlr =
    List.map string_of_ident til
    |> List.map type_of_string in
  match resolve_type_results tlr with
  | Ok tl ->
    type_of_string rts
    |> Result.map (fun t -> TFunc (t, tl))
  | Error err -> Error err

let rec typecheck_statement (s:statement) (tchk:TypeChk.t) (rt) : (TypeChk.t, exn) result =
  match s with
  | Assign (Ident vts, ni, e) ->
    let vtr = type_of_string vts in
    let etr = typecheck_expr tchk e in
    (match vtr, etr with
    | Ok vt, Ok et ->
      if vt = et then
        Ok (TypeChk.add ni et tchk)
      else
        Error Type_mismatch
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
      | Ok TBool, Ok tchk -> typecheck_statement (If l) tchk rt
      | Ok TBool, Error err -> Error err
      | Ok _, _ -> Error Type_mismatch
      | Error err, _ -> Error err))
  | While (e, sl) -> typecheck_statement (If [e, sl]) tchk rt
  | For (a, e, is, sl) ->
    let chk_a = typecheck_statement a tchk rt in
    let new_tchk = Result.value chk_a ~default:tchk in
    let chk_w = typecheck_statement (While (e, sl @ [is])) new_tchk rt in
    (match chk_a, chk_w with
    | Ok _, Ok tchk -> Ok tchk
    | Error err, _ -> Error err
    | _, Error err -> Error err)
  | FuncDef(Ident rts, ni, pl, sl) ->
    let ft = create_function_type_object rts pl in
    (match ft with
    | Ok TFunc (rt, _) ->
      check_statements tchk sl rt
      |> Result.map (fun tchk -> TypeChk.add ni rt tchk)
    | Error err -> Error err
    | Ok _ -> failwith "create_function_type_object didn't return TFunc")
  | Return e ->
    (match typecheck_expr tchk e with
    | Ok t ->
      if t = rt then
        Ok tchk
      else
        Error Type_mismatch
    | Error err -> Error err)
  
  | PrintLn e -> typecheck_statement (Print e) tchk rt
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
