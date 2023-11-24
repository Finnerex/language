open Ast

exception Unimplemented
exception Mismatch_type

module TypeChk = struct
  type t =
  | TypeChk of (Ident.t * e_type) list
  let add i t tchk = 
    match tchk with
    | TypeChk(its) -> TypeChk((i, t)::its)
  let rec gamma i tchk =
    match tchk with
    | TypeChk(its) ->
      (match its with
      | [] -> raise Not_found
      | it::its ->
        let fi, ft = it in
        if fi = i then
          ft
        else
          gamma i (TypeChk its))
end

let rec typecheck_expr (tchk:TypeChk.t) (e:expr) : (e_type, exn) result =
  match e with
  | Int _ -> Ok TInt
  | Bool _ -> Ok TBool
  | EString _ -> Ok TString
  | TypedExpr (t, _) -> Ok t
  | Var i -> Ok (TypeChk.gamma i tchk)
  | Systime -> Ok TInt

  | PreIncr e ->
    Result.bind (typecheck_expr tchk e) (fun t -> if t = TInt then Ok TInt else Error Mismatch_type)
  | PostIncr e ->
    Result.bind (typecheck_expr tchk e) (fun t -> if t = TInt then Ok TInt else Error Mismatch_type)
  
  | And (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TBool, Ok TBool -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | Or (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TBool, Ok TBool -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
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
    | Ok _ -> Error Mismatch_type
    | Error err -> Error err)
  
  | Greater (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | GreaterEq (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | Less (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | LessEq (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TBool
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  
  | Plus (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | Minus (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | Times (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | Div (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  | Modulo (e1, e2) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    (match t1, t2 with
    | Ok TInt, Ok TInt -> Ok TInt
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | _ -> Error Mismatch_type)
  
  | Ternary (e1, e2, e3) ->
    let t1 = typecheck_expr tchk e1 in
    let t2 = typecheck_expr tchk e2 in
    let t3 = typecheck_expr tchk e3 in
    (match t1, t2, t3 with
    | Ok TBool, Ok t2, Ok t3 ->
      if t2 = t3 then
        Ok t2
      else
        Error Mismatch_type
    | Error err, _,  _ -> Error err
    | _, Error err, _ -> Error err
    | _, _, Error err -> Error err
    | _ -> Error Mismatch_type)
