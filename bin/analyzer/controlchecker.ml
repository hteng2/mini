(* controlchecker (ir1 -> ())
    checks location of break, continue, return *)

open Mini

(* Semantic analysis is ctx-sensitive *)
type ctx = { loop : bool; func : bool }

exception CtrlError of Errors.error

(* check existence and infer type *)
let rec check_id id =
  match id with
  | Ir1.IdName name -> ()
  | Ir1.IdAt (id', expr) ->
      check_id id';
      check_expr expr

(* apply type induction rules and convert to ops *)
and check_expr ((expr, t) : Ir1.expr) : unit =
  match expr with
  | Ir1.Int _ | Ir1.Float _ | Ir1.Char _ | Ir1.Str _ | Ir1.Name _ | Ir1.Bool _
  | Ir1.Void ->
      ()
  | Ir1.Neg e | Ir1.Not e -> check_expr e
  | Ir1.Eq (e1, e2)
  | Ir1.Gt (e1, e2)
  | Ir1.Lt (e1, e2)
  | Ir1.Add (e1, e2)
  | Ir1.Sub (e1, e2)
  | Ir1.Mul (e1, e2)
  | Ir1.Div (e1, e2)
  | Ir1.Mod (e1, e2)
  | Ir1.And (e1, e2)
  | Ir1.Or (e1, e2)
  | Ir1.Xor (e1, e2)
  | Ir1.ListAt (e1, e2)
  | Ir1.StrAt (e1, e2) ->
      check_expr e1;
      check_expr e2
  | Ir1.List es -> List.iter (fun e -> check_expr e) es
  | Ir1.FnVal (ps, c, body) -> (
      match t with
      | Types.Fn (t', _) ->
          if
            t' <> Types.Void
            && not (check_dec body { loop = false; func = true })
          then
            raise
              (CtrlError
                 {
                   v = "cannot prove that non-void function will return";
                   span = body.span;
                 })
      | _ -> assert false)
  | Ir1.FnCall (fn, args) ->
      check_expr fn;
      List.iter (fun e -> check_expr e) args

and check_dec d ctx : bool =
  match d.v with
  | Ir1.Let (name, expr) ->
      check_expr expr;
      false
  | Ir1.Var (name, expr) ->
      check_expr expr;
      false
  | Ir1.VarSet (id, expr) ->
      check_id id;
      check_expr expr;
      false
  | Ir1.If (expr, body, body2) -> (
      check_expr expr;
      let r1 = check_dec body ctx in
      match body2 with
      | Some body2' ->
          let r2 = check_dec body2' ctx in
          r1 && r2
      | None -> false)
  | Ir1.While (expr, body) ->
      check_expr expr;
      let _ = check_dec body ctx in
      false
  | Ir1.Break ->
      if ctx.loop then false
      else
        raise (CtrlError { v = "break statement not in loop"; span = d.span })
  | Ir1.Continue ->
      if ctx.loop then false
      else
        raise
          (CtrlError { v = "continue statement not in loop"; span = d.span })
  | Ir1.Return expr ->
      if ctx.func then true
      else
        raise
          (CtrlError { v = "return statement not in function"; span = d.span })
  | Ir1.Block body -> check_decs body ctx

and check_decs ds ctx =
  Array.fold_left (fun r dec -> r || check_dec dec ctx) false ds

let check ds =
  check_decs ds { loop = false; func = false } |> not |> fun x -> assert x
