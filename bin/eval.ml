exception Div_by_0

let rec eval_expr (expr : Ast.expr) (scopes : Values.value Scopes.t list) :
    Values.value =
  match expr.v with
  | Ast.Num n -> Values.Int n
  | Ast.Name name ->
      let v, _ = Scopes.search_scopes scopes name in
      v
  | Ast.True -> Values.Bool true
  | Ast.False -> Values.Bool false
  | Ast.Neg expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n -> Values.Int (-n)
      | _ -> raise (Failure "unreachable"))
  | Ast.Pos expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n -> Values.Int n
      | _ -> raise (Failure "unreachable"))
  | Ast.Not expr' -> (
      match eval_expr expr' scopes with
      | Values.Bool b -> Values.Bool (not b)
      | _ -> raise (Failure "unreachable"))
  | Ast.Eq (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 = v2)
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 = v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Gt (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 > v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Lt (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 < v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Add (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 + v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Sub (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 - v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Mul (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 * v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Div (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int 0 -> raise Div_by_0
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 / v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Mod (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int 0 -> raise Div_by_0
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 mod v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.And (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 && v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Or (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 || v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Xor (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 <> v2)
      | _ -> raise (Failure "unreachable"))
  | Ast.Tuple es -> Values.Tuple (eval_tuple es scopes)

and eval_tuple es scopes =
  match es with
  | [] -> []
  | e :: es' -> eval_expr e scopes :: eval_tuple es' scopes

let rec eval_dec (d : Ast.dec) (scopes : Values.value Scopes.t list) :
    Values.value Scopes.t list =
  match d.v with
  | Ast.Let (name, expr) ->
      Scopes.add_to_scope scopes name Scopes.Const (eval_expr expr scopes)
  | Ast.Var (name, expr) ->
      Scopes.add_to_scope scopes name Scopes.Var (eval_expr expr scopes)
  | Ast.VarSet (name, expr) ->
      Scopes.update_scopes scopes name (eval_expr expr scopes)
  | Ast.Print expr ->
      (match eval_expr expr scopes with
      | Values.Bool v ->
          Printf.printf "%s : bool\n" (if v then "true" else "false")
      | Values.Int v -> Printf.printf "%d : int\n" v
      | Values.Tuple _ -> Printf.printf "- : tuple\n");
      scopes
  | Ast.If (Ast.IfThen (expr, body)) -> (
      match eval_expr expr scopes with
      | Values.Bool true -> (
          match eval body (Scopes.add_scope scopes) with
          | [] -> raise (Failure "unreachable")
          | _ :: scopes' -> scopes')
      | _ -> scopes)
  | Ast.If (Ast.IfThenElse (expr, body, body2)) -> (
      match eval_expr expr scopes with
      | Values.Bool true -> (
          match eval body (Scopes.add_scope scopes) with
          | [] -> raise (Failure "unreachable")
          | _ :: scopes' -> scopes')
      | Values.Bool false -> (
          match eval body2 (Scopes.add_scope scopes) with
          | [] -> raise (Failure "unreachable")
          | _ :: scopes' -> scopes')
      | _ -> raise (Failure "unreachable"))
  | Ast.While (expr, body) ->
      let rec run scopes =
        match eval_expr expr scopes with
        | Values.Bool true -> (
            match eval body (Scopes.add_scope scopes) with
            | [] -> raise (Failure "unreachable")
            | _ :: scopes' -> run scopes')
        | _ -> scopes
      in
      run scopes

and eval (ds : Ast.dec list) (scopes : Values.value Scopes.t list) :
    Values.value Scopes.t list =
  match ds with
  | [] -> scopes
  | d :: ds' ->
      let scope' = eval_dec d scopes in
      eval ds' scope'
