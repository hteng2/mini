(* controlchecker (ir1 -> ())
    checks location of break, continue, return *)

open Mini

(* Semantic analysis is ctx-sensitive *)
type ctx = { loop : bool }

exception CtrlError of Errors.error

(* check existence and infer type *)
let rec check_id id ctx =
  match id with
  | Ir1.IdName name -> ()
  | Ir1.IdAt (id', expr) ->
      check_id id' ctx;
      check_expr expr ctx

(* apply type induction rules and convert to ops *)
and check_expr ({ v = expr, t; span } : Ir1.expr) ctx : unit =
  match expr with
  | Ir1.Int _ | Ir1.Float _ | Ir1.Char _ | Ir1.Str _ | Ir1.Name _ | Ir1.Bool _
  | Ir1.Void ->
      ()
  | Ir1.Neg e | Ir1.Not e -> check_expr e ctx
  | Ir1.Eq (e1, e2)
  | Ir1.Neq (e1, e2)
  | Ir1.Gt (e1, e2)
  | Ir1.Ge (e1, e2)
  | Ir1.Lt (e1, e2)
  | Ir1.Le (e1, e2)
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
      check_expr e1 ctx;
      check_expr e2 ctx
  | Ir1.List es -> List.iter (fun e -> check_expr e ctx) es
  | Ir1.FnVal (ps, c, body) -> check_expr body { loop = false }
  | Ir1.FnCall (fn, args) ->
      check_expr fn ctx;
      List.iter (fun e -> check_expr e ctx) args
  | Ir1.Let (name, expr) -> check_expr expr ctx
  | Ir1.Var (name, expr) -> check_expr expr ctx
  | Ir1.Set (id, expr) ->
      check_id id ctx;
      check_expr expr ctx
  | Ir1.If (expr, body, body2) ->
      check_expr expr ctx;
      check_expr body ctx;
      check_expr body2 ctx
  | Ir1.While (expr, body) ->
      check_expr expr ctx;
      check_expr body { loop = true }
  | Ir1.Break ->
      if not ctx.loop then
        raise (CtrlError { v = "break statement not in loop"; span })
  | Ir1.Continue ->
      if not ctx.loop then
        raise (CtrlError { v = "continue statement not in loop"; span })
  | Ir1.Block body -> check_exprs body ctx

and check_exprs ds ctx = Array.iter (fun e -> check_expr e ctx) ds

let check ds = check_exprs ds { loop = false }
