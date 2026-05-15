module Vars = Map.Make (String)

let force_type t1 t2 t_res = if t1 = t2 then t_res else raise Scopes.TypeError

let rec infer_type (expr : Ast.expr) (scopes : Types.t Scopes.t list) : Types.t
    =
  match expr.v with
  | Ast.Num n -> Types.Int
  | Ast.Name name ->
      let t, _ = Scopes.search_scopes scopes name in
      t
  | Ast.True -> Types.Bool
  | Ast.False -> Types.Bool
  | Ast.Neg e | Ast.Pos e ->
      force_type (infer_type e scopes) Types.Int Types.Int
  | Ast.Not e -> force_type (infer_type e scopes) Types.Bool Types.Bool
  | Ast.Eq (e1, e2) ->
      force_type (infer_type e1 scopes) (infer_type e2 scopes) Types.Bool
  | Ast.Gt (e1, e2) ->
      force_type (infer_type e1 scopes) Types.Int
        (force_type (infer_type e2 scopes) Types.Int Types.Bool)
  | Ast.Lt (e1, e2) ->
      force_type (infer_type e1 scopes) Types.Int
        (force_type (infer_type e2 scopes) Types.Int Types.Bool)
  | Ast.Add (e1, e2)
  | Ast.Sub (e1, e2)
  | Ast.Mul (e1, e2)
  | Ast.Div (e1, e2)
  | Ast.Mod (e1, e2) ->
      force_type (infer_type e1 scopes) Types.Int
        (force_type (infer_type e2 scopes) Types.Int Types.Int)
  | Ast.And (e1, e2) | Ast.Or (e1, e2) | Ast.Xor (e1, e2) ->
      force_type (infer_type e1 scopes) Types.Bool
        (force_type (infer_type e2 scopes) Types.Bool Types.Bool)
  | Ast.Tuple es -> Types.Tuple (infer_tuple es scopes)

and infer_tuple es scopes =
  match es with
  | [] -> []
  | e :: es' -> infer_type e scopes :: infer_tuple es' scopes

let rec infer_dec (d : Ast.dec) (scopes : Types.t Scopes.t list) :
    Types.t Scopes.t list =
  match d.v with
  | Ast.Let (name, expr) ->
      let t = infer_type expr scopes in
      let scopes' = Scopes.add_to_scope scopes name Scopes.Const t in
      scopes'
  | Ast.Var (name, expr) ->
      let t = infer_type expr scopes in
      let scopes' = Scopes.add_to_scope scopes name Scopes.Var t in
      scopes'
  | Ast.VarSet (name, expr) ->
      let t = infer_type expr scopes in
      let t2, m = Scopes.search_scopes scopes name in
      if m = Scopes.Const then raise Scopes.MutError
      else if t = t2 then scopes
      else raise Scopes.TypeError
  | Ast.Print expr ->
      let _ = infer_type expr scopes in
      scopes
  | Ast.If (Ast.IfThen (expr, body)) ->
      let _ = force_type (infer_type expr scopes) Types.Bool Types.Bool in
      let _ = check_program body (Scopes.add_scope scopes) in
      scopes
  | Ast.If (Ast.IfThenElse (expr, body, body2)) ->
      let _ = force_type (infer_type expr scopes) Types.Bool Types.Bool in
      let _ = check_program body (Scopes.add_scope scopes) in
      let _ = check_program body2 (Scopes.add_scope scopes) in
      scopes
  | Ast.While (expr, body) ->
      let _ = force_type (infer_type expr scopes) Types.Bool Types.Bool in
      let _ = check_program body (Scopes.add_scope scopes) in
      scopes

and check_program (ds : Ast.program) (scopes : Types.t Scopes.t list) =
  match ds with
  | [] -> []
  | d :: ds' ->
      let scope' = infer_dec d scopes in
      check_program ds' scope'

let analyze ds = check_program ds (Scopes.add_scope [])
