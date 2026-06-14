module Vars = Map.Make (String)

exception TypeError

let force_type t1 t2 t_res = if t1 = t2 then t_res else raise TypeError

let rec get_type (id : Ast.identifier) (scopes : Types.t Scopes.t list) :
    Types.t =
  match id.v with
  | Ast.IdName name -> Scopes.search_scopes scopes name
  | Ast.IdAt (id', _) -> (
      match get_type id' scopes with
      | Types.List t, m -> (t, m)
      | _ -> raise TypeError)

let rec translate_type (t : Ast.mini_type) =
  match t.v with
  | Ast.MtBase "int" -> Types.Int
  | Ast.MtBase "bool" -> Types.Bool
  | Ast.MtBase "void" -> Types.Void
  | Ast.MtList t' -> Types.List (translate_type t')
  | Ast.MtFn (t, ts) -> Types.Fn (translate_type t, List.map translate_type ts)
  | _ -> assert false

let rec infer_type (expr : Ast.expr) (scopes : Types.t Scopes.t list) : Types.tt
    =
  try
    match expr.v with
    | Ast.Num n -> Types.Int
    | Ast.Name name ->
        let t, _ = Scopes.search_scopes scopes name in
        t
    | Ast.True -> Types.Bool
    | Ast.False -> Types.Bool
    | Ast.Void -> Types.Void
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
    | Ast.At (e1, e2) ->
        force_type (infer_type e2 scopes) Types.Int
          (match infer_type e1 scopes with
          | Types.List t -> t
          | _ -> raise TypeError)
    | Ast.FnVal (ps, t, body) ->
        let scope : Types.t Vars.t =
          List.fold_left
            (fun acc ->
              fun ({ v = name, t; span } : Ast.param) ->
               match Vars.find_opt name acc with
               | None -> Vars.add name (translate_type t, Types.Const) acc
               | Some _ ->
                   raise (Errors.NameError { v = "name already used"; span }))
            Vars.empty ps
        in
        let ts =
          List.map (fun ({ v = _, t } : Ast.param) -> translate_type t) ps
        in
        let t' = translate_type t in
        let _ = infer_dec body (scope :: scopes) (Some t') in
        Types.Fn (t', ts)
    | Ast.FnCall (fn, args) -> (
        match infer_type fn scopes with
        | Types.Fn (t, ts) -> (
            try
              List.fold_left2
                (fun acc ->
                  fun e -> fun t -> force_type t (infer_type e scopes) acc)
                t args ts
            with Invalid_argument _ -> raise TypeError)
        | _ -> raise TypeError)
  with TypeError -> raise (Errors.TypeError { v = (); span = expr.span })

and infer_list es scopes =
  match es with
  | [] -> assert false
  | e :: [] -> infer_type e scopes
  | e :: es' ->
      let t = infer_type e scopes in
      let t2 = infer_list es' scopes in
      if t = t2 then t else raise TypeError

and infer_dec (d : Ast.dec) (scopes : Types.t Scopes.t list)
    (result_type : Types.tt option) : Types.t Scopes.t list =
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
  | Ast.If (expr, body, body2) -> (
      let _ = force_type (infer_type expr scopes) Types.Bool Types.Bool in
      let _ = infer_dec body scopes result_type in
      match body2 with
      | Some body2 ->
          let _ = infer_dec body2 scopes result_type in
          scopes
      | None -> scopes)
  | Ast.While (expr, body) ->
      let _ = force_type (infer_type expr scopes) Types.Bool Types.Bool in
      let _ = infer_dec body scopes result_type in
      scopes
  | Ast.Break | Ast.Continue -> scopes
  | Ast.Return expr -> (
      let t1 = infer_type expr scopes in
      match result_type with
      | None -> scopes
      | Some t ->
          let _ = force_type t t1 t in
          scopes)
  | Ast.Block body ->
      let _ = check_program body (Scopes.add_scope scopes) result_type in
      scopes

and check_program (ds : Ast.program) (scopes : Types.t Scopes.t list)
    (result_type : Types.tt option) =
  match ds with
  | [] -> ()
  | d :: ds' ->
      let scope' = infer_dec d scopes result_type in
      check_program ds' scope' result_type

let analyze ds = check_program ds (Scopes.add_scope []) None
