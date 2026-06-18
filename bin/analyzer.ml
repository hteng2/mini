type ctx = { loop : bool; func : Types.tt option }

exception TypeError of string
exception CtrlError

let force_type t1 t2 =
  if t1 = t2 then () else raise (TypeError "failed force_type")

let rec get_type (id : Ast.identifier) (scopes : Types.t Scopes.t list) :
    Types.t =
  match id.v with
  | Ast.IdName name -> Scopes.search_scopes scopes name
  | Ast.IdAt (id', _) -> (
      match get_type id' scopes with
      | Types.List t, m -> (t, m)
      | _ -> raise (TypeError "list access of non-list"))

let rec translate_type (t : Ast.mini_type) =
  match t.v with
  | Ast.MtBase "int" -> Types.Int
  | Ast.MtBase "bool" -> Types.Bool
  | Ast.MtBase "void" -> Types.Void
  | Ast.MtList t' -> Types.List (translate_type t')
  | Ast.MtFn (t, ts) -> Types.Fn (translate_type t, List.map translate_type ts)
  | _ -> assert false

let rec analyze_expr (expr : Ast.expr) (scopes : Types.t Scopes.t list) :
    Types.tt =
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
        let t = analyze_expr e scopes in
        force_type t Types.Int;
        Types.Int
    | Ast.Not e ->
        let t = analyze_expr e scopes in
        force_type t Types.Bool;
        Types.Int
    | Ast.Eq (e1, e2) ->
        let t1 = analyze_expr e1 scopes in
        let t2 = analyze_expr e2 scopes in
        force_type t1 t2;
        Types.Bool
    | Ast.Gt (e1, e2) ->
        force_type (analyze_expr e1 scopes) Types.Int;
        force_type (analyze_expr e2 scopes) Types.Int;
        Types.Bool
    | Ast.Lt (e1, e2) ->
        force_type (analyze_expr e1 scopes) Types.Int;
        force_type (analyze_expr e2 scopes) Types.Int;
        Types.Bool
    | Ast.Add (e1, e2)
    | Ast.Sub (e1, e2)
    | Ast.Mul (e1, e2)
    | Ast.Div (e1, e2)
    | Ast.Mod (e1, e2) ->
        force_type (analyze_expr e1 scopes) Types.Int;
        force_type (analyze_expr e2 scopes) Types.Int;
        Types.Int
    | Ast.And (e1, e2) | Ast.Or (e1, e2) | Ast.Xor (e1, e2) ->
        force_type (analyze_expr e1 scopes) Types.Bool;
        force_type (analyze_expr e2 scopes) Types.Bool;
        Types.Bool
    | Ast.List es -> Types.List (infer_list es scopes)
    | Ast.At (e1, e2) -> (
        force_type (analyze_expr e2 scopes) Types.Int;
        match analyze_expr e1 scopes with
        | Types.List t -> t
        | _ -> raise (TypeError "list access of non-list"))
    | Ast.FnVal (ps, t, body) ->
        let scopes' = Scopes.add_scope scopes in
        let _ =
          List.iter
            (fun ({ v = name, t; span } : Ast.param) ->
              match Scopes.search_top scopes' name with
              | None ->
                  if name = "_" then ()
                  else
                    Scopes.add_to_scope scopes' name
                      (translate_type t, Types.Var)
              | Some _ ->
                  raise
                    (Errors.NameError { v = "param name already used"; span }))
            ps
        in
        let ts =
          List.map (fun ({ v = _, t } : Ast.param) -> translate_type t) ps
        in
        let t' = translate_type t in
        let _ = infer_dec body scopes' { loop = false; func = Some t' } in
        Types.Fn (t', ts)
    | Ast.FnCall (fn, args) -> (
        match analyze_expr fn scopes with
        | Types.Fn (t, ts) -> (
            try
              List.iter2
                (fun arg -> fun t -> force_type t (analyze_expr arg scopes))
                args ts;
              t
            with Invalid_argument _ ->
              raise (TypeError "argument count invalid"))
        | _ -> raise (TypeError "call of non-function"))
  with TypeError e -> raise (Errors.TypeError { v = e; span = expr.span })

and infer_list es scopes =
  match es with
  | [] -> assert false
  | e :: [] -> analyze_expr e scopes
  | e :: es' ->
      let t = analyze_expr e scopes in
      let t2 = infer_list es' scopes in
      if t = t2 then t else raise (TypeError "list types do not match")

and infer_dec (d : Ast.dec) (scopes : Types.t Scopes.t list) (ctx : ctx) : unit
    =
  match d.v with
  | Ast.Let (name, expr) ->
      let t = analyze_expr expr scopes in
      (match Scopes.search_top scopes name with
      | Some (_, Types.Const) -> ()
      | None -> ()
      | _ -> raise (TypeError "let statement must shadow other let statements"));
      if name = "_" then () else Scopes.add_to_scope scopes name (t, Types.Const)
  | Ast.Var (name, expr) ->
      let t = analyze_expr expr scopes in
      (match Scopes.search_top scopes name with
      | Some _ -> raise (Scopes.NameError name)
      | None -> ());
      if name = "_" then () else Scopes.add_to_scope scopes name (t, Types.Var)
  | Ast.VarSet (id, expr) ->
      let t = analyze_expr expr scopes in
      let t2, m = get_type id scopes in
      if m = Types.Var && t = t2 then ()
      else raise (TypeError "cannot reassign non-variables")
  | Ast.Print expr ->
      let _ = analyze_expr expr scopes in
      ()
  | Ast.Println expr ->
      let _ = analyze_expr expr scopes in
      ()
  | Ast.If (expr, body, body2) -> (
      force_type (analyze_expr expr scopes) Types.Bool;
      infer_dec body scopes ctx;
      match body2 with Some body2 -> infer_dec body2 scopes ctx | None -> ())
  | Ast.While (expr, body) ->
      force_type (analyze_expr expr scopes) Types.Bool;
      infer_dec body scopes { loop = true; func = ctx.func }
  | Ast.Break | Ast.Continue -> if ctx.loop then () else raise CtrlError
  | Ast.Return expr -> (
      match ctx.func with
      | None -> raise CtrlError
      | Some t ->
          let t1 = analyze_expr expr scopes in
          force_type t t1)
  | Ast.Block body -> check_program body (Scopes.add_scope scopes) ctx

and check_program (ds : Ast.program) (scopes : Types.t Scopes.t list)
    (ctx : ctx) =
  match ds with
  | [] -> ()
  | d :: ds' ->
      infer_dec d scopes ctx;
      check_program ds' scopes ctx

let analyze ds =
  check_program ds (Scopes.add_scope []) { loop = false; func = None }
