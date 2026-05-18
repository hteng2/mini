module Vars = Map.Make (String)

exception TypeError

let force_type t1 t2 t_res = if t1 = t2 then t_res else raise TypeError

let rec get_type (v : Ast.v) (scopes : Types.t Scopes.t list) : Types.t =
  match v with
  | Ast.Name name -> Scopes.search_scopes scopes name
  | Ast.At (v', _) -> (
      match get_type v' scopes with
      | Types.List t, m -> (t, m)
      | _ -> raise TypeError)

let rec infer_type (expr : Ast.expr) (scopes : Types.t Scopes.t list) : Types.tt
    =
  try
    match expr.v with
    | Ast.Num n -> Types.Int
    | Ast.Id id ->
        let t, _ = get_type id scopes in
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
    | Ast.List es -> Types.List (infer_list es scopes)
  with TypeError -> raise (Errors.TypeError { v = (); span = expr.span })

and infer_list es scopes =
  match es with
  | [] -> assert false
  | e :: [] -> infer_type e scopes
  | e :: es' ->
      let t = infer_type e scopes in
      let t2 = infer_list es' scopes in
      if t = t2 then t else raise TypeError

let rec infer_dec (d : Ast.dec) (scopes : Types.t Scopes.t list) :
    Types.t Scopes.t list =
  match d.v with
  | Ast.Let (name, expr) ->
      let t = infer_type expr scopes in
      (match Scopes.search_top scopes name with
      | Some (_, Types.Const) -> ()
      | None -> ()
      | _ -> raise TypeError);
      let scopes' = Scopes.add_to_scope scopes name (t, Types.Const) in
      scopes'
  | Ast.Var (name, expr) ->
      let t = infer_type expr scopes in
      (match Scopes.search_top scopes name with
      | Some _ -> raise Scopes.NameError
      | None -> ());
      let scopes' = Scopes.add_to_scope scopes name (t, Types.Var) in
      scopes'
  | Ast.VarSet (id, expr) ->
      let t = infer_type expr scopes in
      let t2, m = get_type id scopes in
      if m = Types.Var && t = t2 then scopes else raise TypeError
  | Ast.Print expr ->
      let _ = infer_type expr scopes in
      scopes
  | Ast.Println expr ->
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
