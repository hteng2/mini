module Vars = Map.Make (String)

exception TypeError
exception NameError

let force_type t1 t2 t_res = if t1 = t2 then t_res else raise TypeError

let rec infer_type (expr : Ast.expr) (scope : Types.mini_type Vars.t) :
    Types.mini_type =
  match expr with
  | Ast.Num n -> Types.Int
  | Ast.Name name -> (
      try Vars.find name scope with Not_found -> raise NameError)
  | Ast.True -> Types.Bool
  | Ast.False -> Types.Bool
  | Ast.Neg e | Ast.Pos e -> force_type (infer_type e scope) Types.Int Types.Int
  | Ast.Not e -> force_type (infer_type e scope) Types.Bool Types.Bool
  | Ast.Eq (e1, e2) ->
      force_type (infer_type e1 scope) (infer_type e2 scope) Types.Bool
  | Ast.Add (e1, e2) | Ast.Sub (e1, e2) | Ast.Mul (e1, e2) | Ast.Div (e1, e2) ->
      force_type (infer_type e1 scope) Types.Int
        (force_type (infer_type e2 scope) Types.Int Types.Int)
  | Ast.And (e1, e2) | Ast.Or (e1, e2) | Ast.Xor (e1, e2) ->
      force_type (infer_type e1 scope) Types.Bool
        (force_type (infer_type e2 scope) Types.Bool Types.Bool)

let rec check_dec (d : Ast.expr Ast.dec) (scope : Types.mini_type Vars.t) :
    Ast.expr Types.typed Ast.dec * Types.mini_type Vars.t =
  match d with
  | Ast.Let (name, expr) ->
      let t = infer_type expr scope in
      let scope' = Vars.add name t scope in
      (Ast.Let (name, (t, expr)), scope')
  | Ast.Print expr ->
      let t = infer_type expr scope in
      (Ast.Print (t, expr), scope)
  | Ast.If (expr, body) ->
      let t = force_type (infer_type expr scope) Types.Bool Types.Bool in
      let body' = check_program body scope in
      (Ast.If ((t, expr), body'), scope)

and check_program (ds : Ast.expr Ast.dec list) (scope : Types.mini_type Vars.t)
    =
  match ds with
  | [] -> []
  | d :: ds' ->
      let d', scope' = check_dec d scope in
      d' :: check_program ds' scope'

let analyze ds = check_program ds Vars.empty
