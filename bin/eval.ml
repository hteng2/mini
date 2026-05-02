module Vars = Map.Make (String)

exception Div_by_0
exception NameError

let rec eval_expr (expr : Ast.expr) (scope : Values.value Vars.t) : Values.value
    =
  match expr with
  | Ast.Num n -> Values.Int n
  | Ast.Name name -> (
      try Vars.find name scope with Not_found -> raise NameError)
  | Ast.True -> Values.Bool true
  | Ast.False -> Values.Bool false
  | Ast.Neg expr' -> (
      match eval_expr expr' scope with
      | Values.Int n -> Values.Int (-n)
      | _ -> raise (Failure "unreachable"))
  | Ast.Pos expr' -> (
      match eval_expr expr' scope with
      | Values.Int n -> Values.Int n
      | _ -> raise (Failure "unreachable"))
  | Ast.Not expr' -> (
      match eval_expr expr' scope with
      | Values.Bool b -> Values.Bool (not b)
      | _ -> raise (Failure "unreachable"))
  | Ast.Eq (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 = v2)
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 = v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Add (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 + v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Sub (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 - v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Mul (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 * v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Div (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Int v1, Values.Int 0 -> raise Div_by_0
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 / v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.And (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 && v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Or (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 || v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Xor (e1, e2) -> (
      match (eval_expr e1 scope, eval_expr e2 scope) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 <> v2)
      | _ -> raise (Failure "unreachable"))

let rec eval_dec (d : Ast.expr Types.typed Ast.dec)
    (scope : Values.value Vars.t) : Values.value Vars.t =
  match d with
  | Ast.Let (name, (t, expr)) -> Vars.add name (eval_expr expr scope) scope
  | Ast.Print (t, expr) ->
      (match eval_expr expr scope with
      | Values.Bool v ->
          Printf.printf "%s : bool\n" (if v then "true" else "false")
      | Values.Int v -> Printf.printf "%d : int\n" v);
      scope
  | Ast.If ((t, expr), body) -> (
      match eval_expr expr scope with
      | Values.Bool true ->
          let _ = eval body scope in
          scope
      | _ -> scope)

and eval (ds : Ast.expr Types.typed Ast.dec list) (scope : Values.value Vars.t)
    : Values.value Vars.t =
  match ds with
  | [] -> scope
  | d :: ds' ->
      let scope' = eval_dec d scope in
      eval ds' scope'
