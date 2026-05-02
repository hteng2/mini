module Vars = Map.Make (String)

exception TypeError

let force_type t1 t2 t_res = if t1 = t2 then t_res else raise TypeError

let rec infer_type (expr : Ast.expr) (scopes : Types.mini_type Scopes.t list) :
    Types.mini_type =
  match expr with
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
  | Ast.Add (e1, e2) | Ast.Sub (e1, e2) | Ast.Mul (e1, e2) | Ast.Div (e1, e2) ->
      force_type (infer_type e1 scopes) Types.Int
        (force_type (infer_type e2 scopes) Types.Int Types.Int)
  | Ast.And (e1, e2) | Ast.Or (e1, e2) | Ast.Xor (e1, e2) ->
      force_type (infer_type e1 scopes) Types.Bool
        (force_type (infer_type e2 scopes) Types.Bool Types.Bool)

let rec infer_dec (d : Ast.expr Ast.dec)
    (scopes : Types.mini_type Scopes.t list) :
    Ast.expr Types.typed Ast.dec * Types.mini_type Scopes.t list =
  match d with
  | Ast.Let (name, expr) ->
      let t = infer_type expr scopes in
      let scopes' = Scopes.add_to_scope scopes name Scopes.Const t in
      (Ast.Let (name, (t, expr)), scopes')
  | Ast.Var (name, expr) ->
      let t = infer_type expr scopes in
      let scopes' = Scopes.add_to_scope scopes name Scopes.Var t in
      (Ast.Var (name, (t, expr)), scopes')
  | Ast.VarSet (name, expr) ->
      let t = infer_type expr scopes in
      let t2, m = Scopes.search_scopes scopes name in
      if m = Scopes.Const then raise Scopes.MutError
      else if t = t2 then (Ast.VarSet (name, (t, expr)), scopes)
      else raise TypeError
  | Ast.Print expr ->
      let t = infer_type expr scopes in
      (Ast.Print (t, expr), scopes)
  | Ast.If (expr, body) ->
      let t = force_type (infer_type expr scopes) Types.Bool Types.Bool in
      let body' = check_program body (Scopes.add_scope scopes) in
      (Ast.If ((t, expr), body'), scopes)

and check_program (ds : Ast.expr Ast.dec list)
    (scopes : Types.mini_type Scopes.t list) =
  match ds with
  | [] -> []
  | d :: ds' ->
      let d', scope' = infer_dec d scopes in
      d' :: check_program ds' scope'

let analyze ds = check_program ds (Scopes.add_scope [])
