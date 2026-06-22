let bind_expect f e x =
  match f x with Some y -> y | _ -> raise (Errors.Expected e)

let bind_x x =
  bind_expect (fun (ts : Token.t Stream.t) ->
      match Stream.pop ts with
      | Stream.Head ({ v = x; span }, ts') -> Some (ts', span)
      | _ -> None)

let bind_name e x =
  bind_expect
    (fun (ts : Token.t Stream.t) ->
      match Stream.pop ts with
      | Stream.Head ({ v = Token.Name name }, ts') -> Some (ts', name)
      | _ -> None)
    e x

let rec parse_type (ts : Token.t Stream.t) :
    Token.t Stream.t * Ast.mini_type option =
  let front = Stream.pop ts in
  match front with
  | Stream.Head ({ v = Token.Lparen; span }, ts') -> (
      let ts'', t_opt = parse_type ts' in
      match t_opt with
      | None ->
          let old_ts = Stream.push (fun () -> front) in
          (old_ts, None)
      | Some t ->
          let ts''', (_, end_loc) =
            bind_x Token.Rparen { v = "matching )"; span } ts''
          in
          let start_loc, _ = span in
          (ts''', Some { v = t.v; span = (start_loc, end_loc) }))
  | Stream.Head ({ v = Token.Name name; span }, ts') ->
      let ts'', t =
        advance ts' ({ v = Ast.MtBase name; span } : Ast.mini_type)
      in
      (ts'', Some t)
  | _ ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, None)

and advance (ts : Token.t Stream.t) (t : Ast.mini_type) :
    Token.t Stream.t * Ast.mini_type =
  let front = Stream.pop ts in
  match front with
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let ts'', types = parse_types ts' in
      let ts''', (_, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let start_loc, _ = span in
      advance ts''' { v = Ast.MtFn (t, types); span = (start_loc, end_loc) }
  | Stream.Head (({ v = Token.Lbrack; span = start_loc, _ } as token), ts') -> (
      let front = Stream.pop ts' in
      match front with
      | Stream.Head ({ v = Token.Rbrack; span = _, end_loc }, ts'') ->
          advance ts'' { v = Ast.MtList t; span = (start_loc, end_loc) }
      | _ ->
          let old_ts' = Stream.push (fun () -> front) in
          let old_ts = Stream.push (fun () -> Stream.Head (token, old_ts')) in
          (old_ts, t))
  | _ ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, t)

and parse_types (ts : Token.t Stream.t) =
  match parse_type ts with
  | ts', None -> (ts', [])
  | ts', Some t -> (
      let front = Stream.pop ts' in
      match front with
      | Stream.Head ({ v = Token.Comma }, ts'') ->
          let ts''', types = parse_types ts'' in
          (ts''', t :: types)
      | _ ->
          let old_ts' = Stream.push (fun () -> front) in
          (old_ts', [ t ]))

and bind_type e x =
  bind_expect
    (fun ts ->
      match parse_type ts with ts', None -> None | ts', Some t -> Some (ts', t))
    e x

let rec parse_param (ts : Token.t Stream.t) :
    Token.t Stream.t * Ast.param option =
  let front = Stream.pop ts in
  match front with
  | Stream.Head (({ v = Token.Name name; span = start_loc, _ } as token), ts')
    -> (
      match parse_type ts' with
      | ts'', Some mt ->
          let _, end_loc = mt.span in
          (ts'', Some { v = (name, mt); span = (start_loc, end_loc) })
      | ts'', None ->
          let old_ts = Stream.push (fun () -> Stream.Head (token, ts'')) in
          (old_ts, None))
  | _ ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, None)

let rec parse_params (ts : Token.t Stream.t) : Token.t Stream.t * Ast.param list
    =
  match parse_param ts with
  | ts', None -> (ts', [])
  | ts', Some param -> (
      let front = Stream.pop ts' in
      match front with
      | Stream.Head ({ v = Token.Comma }, ts'') ->
          let ts''', ps = parse_params ts'' in
          (ts''', param :: ps)
      | _ ->
          let old_ts' = Stream.push (fun () -> front) in
          (old_ts', [ param ]))

let prefix_bp = 13

let is_prefix op =
  match op with Token.Sub | Token.Add | Token.Not -> true | _ -> false

let prefix_combine ({ v = op; span = start_loc, _ } : Token.t)
    ({ span = _, end_loc } as r : Ast.expr) : Ast.expr option =
  let v =
    match op with
    | Token.Sub -> Some (Ast.Neg r)
    | Token.Add -> Some (Ast.Pos r)
    | Token.Not -> Some (Ast.Not r)
    | _ -> None
  in
  Option.bind v (fun v -> Some ({ v; span = (start_loc, end_loc) } : Ast.expr))

let bp op =
  match op with
  | Token.Or -> Some (1, 2)
  | Token.Xor -> Some (3, 4)
  | Token.And -> Some (5, 6)
  | Token.Eq | Token.Gt | Token.Lt -> Some (7, 8)
  | Token.Add | Token.Sub -> Some (9, 10)
  | Token.Mul | Token.Div | Token.Mod -> Some (11, 12)
  | _ -> None

let combine ({ v = _; span = start_loc, _ } as l : Ast.expr)
    ({ v = op } : Token.t) ({ v = _; span = _, end_loc } as r : Ast.expr) :
    Ast.expr option =
  let v =
    match op with
    | Token.Or -> Some (Ast.Or (l, r))
    | Token.Xor -> Some (Ast.Xor (l, r))
    | Token.And -> Some (Ast.And (l, r))
    | Token.Eq -> Some (Ast.Eq (l, r))
    | Token.Gt -> Some (Ast.Gt (l, r))
    | Token.Lt -> Some (Ast.Lt (l, r))
    | Token.Add -> Some (Ast.Add (l, r))
    | Token.Sub -> Some (Ast.Sub (l, r))
    | Token.Mul -> Some (Ast.Mul (l, r))
    | Token.Div -> Some (Ast.Div (l, r))
    | Token.Mod -> Some (Ast.Mod (l, r))
    | _ -> None
  in
  Option.bind v (fun v -> Some ({ v; span = (start_loc, end_loc) } : Ast.expr))

let rec bind_expr min_bp =
  bind_expect (fun ts ->
      match parse_expr ts min_bp with
      | ts', Some expr -> Some (ts', expr)
      | _ -> None)

and parse_expr (ts : Token.t Stream.t) (min_bp : int) :
    Token.t Stream.t * Ast.expr option =
  let front = Stream.pop ts in
  match front with
  | Stream.End ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, None)
  | Stream.Head (({ v; span } as token), ts') -> (
      match v with
      | Token.Num n ->
          let ts'', expr = advance_expr ts' min_bp { v = Ast.Num n; span } in
          (ts'', Some expr)
      | Token.Name n ->
          let ts'', expr = advance_expr ts' min_bp { v = Ast.Name n; span } in
          (ts'', Some expr)
      | Token.True ->
          let ts'', expr = advance_expr ts' min_bp { v = Ast.True; span } in
          (ts'', Some expr)
      | Token.False ->
          let ts'', expr = advance_expr ts' min_bp { v = Ast.False; span } in
          (ts'', Some expr)
      | Token.Lparen -> (
          let front' = Stream.pop ts' in
          match front' with
          | Stream.Head ({ v = Token.Rparen; span = _, end_loc }, ts'') ->
              let start_loc, _ = span in
              let ts''', expr =
                advance_expr ts'' min_bp
                  { v = Ast.Void; span = (start_loc, end_loc) }
              in
              (ts''', Some expr)
          | _ ->
              let old_ts' = Stream.push (fun () -> front') in
              let ts'', (inner : Ast.expr) =
                bind_expr 0 { v = "following expression"; span } old_ts'
              in
              let ts''', span2 =
                bind_x Token.Rparen { v = "closing )"; span } ts''
              in
              let start_loc, _ = span in
              let _, end_loc = span2 in
              let ts'''', expr =
                advance_expr ts''' min_bp
                  { v = inner.v; span = (start_loc, end_loc) }
              in
              (ts'''', Some expr))
      | Token.Lbrack ->
          let ts'', es = parse_exprs ts' in
          let ts''', (_, end_loc) =
            bind_x Token.Rbrack { v = "closing bracket"; span } ts''
          in
          let start_loc, _ = span in
          let ts'''', expr =
            advance_expr ts''' min_bp
              { v = Ast.List es; span = (start_loc, end_loc) }
          in
          (ts'''', Some expr)
      | Token.Fn ->
          let ts2, lspan =
            bind_x Token.Lparen { v = "param type spec"; span } ts'
          in
          let ts3, ps = parse_params ts2 in
          let ts4, _ =
            bind_x Token.Rparen { v = "matching )"; span = lspan } ts3
          in
          let ts5, t = bind_type { v = "result type spec"; span } ts4 in
          let ts6, (body : Ast.dec) =
            bind_dec ({ v = "body"; span } : Errors.error) ts5
          in
          let start_loc, _ = span in
          let _, end_loc = body.span in
          let ts6, expr =
            advance_expr ts6 min_bp
              { v = Ast.FnVal (ps, t, body); span = (start_loc, end_loc) }
          in
          (ts6, Some expr)
      | v when is_prefix v -> (
          let ts'', inner =
            bind_expr prefix_bp { v = "following expression"; span } ts'
          in
          let inner' = prefix_combine token inner in
          match inner' with
          | None -> assert false
          | Some inner' ->
              let ts''', expr = advance_expr ts'' min_bp inner' in
              (ts''', Some expr))
      | _ ->
          let old_ts = Stream.push (fun () -> front) in
          (old_ts, None))

and advance_expr (ts : Token.t Stream.t) (min_bp : int) (left : Ast.expr) :
    Token.t Stream.t * Ast.expr =
  let front = Stream.pop ts in
  match front with
  | Stream.End ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, left)
  | Stream.Head ({ v = Token.Lbrack; span }, ts') ->
      let ts'', inner = bind_expr 0 { v = "following expression"; span } ts' in
      let ts''', (_, end_loc) =
        bind_x Token.Rbrack { v = "matching ]"; span } ts''
      in
      let start_loc, _ = left.span in
      advance_expr ts''' min_bp
        { v = Ast.At (left, inner); span = (start_loc, end_loc) }
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let ts'', es = parse_exprs ts' in
      let ts''', (_, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let start_loc, _ = span in
      advance_expr ts''' min_bp
        { v = Ast.FnCall (left, es); span = (start_loc, end_loc) }
  | Stream.Head (({ v; span } as token), ts') -> (
      match bp v with
      | Some (l, r) when min_bp < l -> (
          let ts'', right = bind_expr r { v = "right expression"; span } ts' in
          match combine left token right with
          | None -> assert false
          | Some left' -> advance_expr ts'' min_bp left')
      | _ ->
          let old_ts = Stream.push (fun () -> front) in
          (old_ts, left))

and parse_exprs (ts : Token.t Stream.t) : Token.t Stream.t * Ast.expr list =
  match parse_expr ts 0 with
  | ts', None -> (ts', [])
  | ts', Some expr -> (
      let front = Stream.pop ts' in
      match front with
      | Stream.Head ({ v = Token.Comma }, ts'') ->
          let ts''', es = parse_exprs ts'' in
          (ts''', expr :: es)
      | _ ->
          let old_ts' = Stream.push (fun () -> front) in
          (old_ts', [ expr ]))

and bind_id g x =
  bind_expect
    (fun ts ->
      match parse_id ts with ts', None -> None | ts', Some id -> Some (ts', id))
    g x

and parse_id (ts : Token.t Stream.t) : Token.t Stream.t * Ast.identifier option
    =
  let front = Stream.pop ts in
  match front with
  | Stream.Head ({ v = Token.Name name; span }, ts') ->
      let ts'', id = advance_id ts' { v = Ast.IdName name; span } in
      (ts'', Some id)
  | _ ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, None)

and advance_id (ts : Token.t Stream.t) (left : Ast.identifier) :
    Token.t Stream.t * Ast.identifier =
  let front = Stream.pop ts in
  match front with
  | Stream.Head ({ v = Token.Lbrack; span }, ts') ->
      let ts'', inner = bind_expr 0 { v = "following expression"; span } ts' in
      let ts''', (_, end_loc) =
        bind_x Token.Rbrack { v = "closing bracket"; span } ts''
      in
      let start_loc, _ = left.span in
      advance_id ts'''
        { v = Ast.IdAt (left, inner); span = (start_loc, end_loc) }
  | _ ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, left)

and parse_dec (ts : Token.t Stream.t) : Token.t Stream.t * Ast.dec option =
  let front = Stream.pop ts in
  match front with
  | Stream.Head ({ v = Token.Let; span = start_loc, end_loc }, ts1) ->
      let ts2, name =
        bind_name { v = "name"; span = (start_loc, end_loc) } ts1
      in
      let ts3, (_, end_loc) =
        (bind_x Token.Eq) { v = "'='"; span = (start_loc, end_loc) } ts2
      in
      let ts4, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts3
      in
      (ts4, Some { v = Ast.Let (name, expr); span = (start_loc, end_loc) })
  | Stream.Head ({ v = Token.Var; span = start_loc, end_loc }, ts1) ->
      let ts2, name =
        bind_name { v = "name"; span = (start_loc, end_loc) } ts1
      in
      let ts3, (_, end_loc) =
        (bind_x Token.Eq) { v = "'='"; span = (start_loc, end_loc) } ts2
      in
      let ts4, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts3
      in
      (ts4, Some { v = Ast.Var (name, expr); span = (start_loc, end_loc) })
  | Stream.Head ({ v = Token.Print; span = start_loc, end_loc }, ts1) ->
      let ts2, expr =
        (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts1
      in
      (ts2, Some { v = Ast.Print expr; span = (start_loc, end_loc) })
  | Stream.Head ({ v = Token.Println; span = start_loc, end_loc }, ts1) ->
      let ts2, expr =
        (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts1
      in
      (ts2, Some { v = Ast.Println expr; span = (start_loc, end_loc) })
  | Stream.Head ({ v = Token.If; span = start_loc, end_loc }, ts1) -> (
      let ts2, expr =
        (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts1
      in
      let ts3, ({ span = _, end_loc } as body : Ast.dec) =
        bind_dec { v = "body"; span = (start_loc, end_loc) } ts2
      in
      let front = Stream.pop ts3 in
      match front with
      | Stream.Head ({ v = Token.Else; span = _, end_loc; _ }, ts4) ->
          let ts5, ({ span = _, end_loc } as body2 : Ast.dec) =
            bind_dec { v = "body"; span = (start_loc, end_loc) } ts4
          in
          ( ts5,
            Some
              {
                v = Ast.If (expr, body, Some body2);
                span = (start_loc, end_loc);
              } )
      | _ ->
          let old_ts3 = Stream.push (fun () -> front) in
          ( old_ts3,
            Some { v = Ast.If (expr, body, None); span = (start_loc, end_loc) }
          ))
  | Stream.Head ({ v = Token.While; span = start_loc, end_loc }, ts1) ->
      let ts2, expr =
        (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts1
      in
      let ts3, ({ span = _, end_loc } as body : Ast.dec) =
        bind_dec { v = "body"; span = (start_loc, end_loc) } ts2
      in
      (ts3, Some { v = Ast.While (expr, body); span = (start_loc, end_loc) })
  | Stream.Head ({ v = Token.Break; span }, ts') ->
      (ts', Some { v = Ast.Break; span })
  | Stream.Head ({ v = Token.Continue; span }, ts') ->
      (ts', Some { v = Ast.Continue; span })
  | Stream.Head ({ v = Token.Return; span }, ts1) ->
      let ts2, expr = (bind_expr 0) { v = "expression"; span } ts1 in
      let start_loc, _ = span in
      let _, end_loc = expr.span in
      (ts2, Some { v = Ast.Return expr; span = (start_loc, end_loc) })
  | Stream.Head ({ v = Token.Lbrace; span = start_loc, end_loc }, ts1) ->
      let ts2, body = p ts1 in
      let ts3, (_, end_loc) =
        (bind_x Token.Rbrace) { v = "'}'"; span = (start_loc, end_loc) } ts2
      in
      (ts3, Some { v = Ast.Block body; span = (start_loc, end_loc) })
  | front -> (
      let old_ts = Stream.push (fun () -> front) in
      let ts1, id_opt = parse_id old_ts in
      match id_opt with
      | None -> (ts1, None)
      | Some id ->
          let start_loc, end_loc = id.span in
          let ts2, (_, end_loc) =
            bind_x Token.Eq { v = "'='"; span = (start_loc, end_loc) } ts1
          in
          let ts3, ({ span = _, end_loc; _ } as expr : Ast.expr) =
            (bind_expr 0) { v = "expression"; span = (start_loc, end_loc) } ts2
          in
          (ts3, Some { v = Ast.VarSet (id, expr); span = (start_loc, end_loc) })
      )

and bind_dec e x =
  bind_expect
    (fun ts ->
      match parse_dec ts with ts', None -> None | ts', Some d -> Some (ts', d))
    e x

and p (ts : Token.t Stream.t) : Token.t Stream.t * Ast.dec list =
  let front = Stream.pop ts in
  match front with
  | Stream.End ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, [])
  | Stream.Head ({ v = Token.Rbrace }, _) ->
      let old_ts = Stream.push (fun () -> front) in
      (old_ts, [])
  | Stream.Head ({ span }, _) ->
      let old_ts = Stream.push (fun () -> front) in
      let ts', d = bind_dec { v = "declaration"; span } old_ts in
      let ts'', ds = p ts' in
      (ts'', d :: ds)

let parse ts =
  let ts', ds = p ts in
  match Stream.pop ts' with
  | Stream.End -> ds
  | Stream.Head ({ span }, _) -> raise (Errors.Unexpected { v = "token"; span })
