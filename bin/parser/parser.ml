open Mini

let rec scan_imports (ts : Token.t Stream.t) =
  match ts () with
  | Stream.Head ({ v = Token.Import; span = span1 }, ts') -> (
      match ts' () with
      | Stream.Head ({ v = Token.Str s; span = span2 }, ts'') -> (
          match ts'' () with
          | Stream.Head ({ v = Token.Semicolon; span = span3 }, ts''') ->
              let ts''', is = scan_imports ts'' in
              (ts''', s :: is)
          | _ -> assert false)
      | _ -> assert false)
  | front -> ((fun () -> front), [])

let bind_expect f e x =
  match f x with Some y -> y | _ -> raise (Errors.Expected e)

let bind_x x =
  bind_expect (fun (ts : Token.t Stream.t) ->
      match ts () with
      | Stream.Head ({ v; span }, ts') when v = x -> Some (ts', span)
      | _ -> None)

let bind_name e x =
  bind_expect
    (fun (ts : Token.t Stream.t) ->
      match ts () with
      | Stream.Head ({ v = Token.Name name }, ts') -> Some (ts', name)
      | _ -> None)
    e x

let rec parse_type (ts : Token.t Stream.t) :
    Token.t Stream.t * Ast.mini_type option =
  let front = ts () in
  match front with
  | Stream.Head ({ v = Token.Lparen; span }, ts') -> (
      let ts'', t_opt = parse_type ts' in
      match t_opt with
      | None ->
          let old_ts = fun () -> front in
          (old_ts, None)
      | Some t ->
          let ts''', (sn, _, end_loc) =
            bind_x Token.Rparen { v = "matching )"; span } ts''
          in
          let sn, start_loc, _ = span in
          (ts''', Some { v = t.v; span = (sn, start_loc, end_loc) }))
  | Stream.Head ({ v = Token.Name name; span }, ts') ->
      let ts'', t = advance_type ts' { v = Ast.MtBase name; span } in
      (ts'', Some t)
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, None)

and advance_type ts (t : Ast.mini_type) =
  let front = ts () in
  match front with
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let ts'', types = parse_types ts' in
      let ts''', (sn, _, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let sn, start_loc, _ = span in
      advance_type ts'''
        { v = Ast.MtFn (t, types); span = (sn, start_loc, end_loc) }
  | Stream.Head (({ v = Token.Lbrack; span = sn, start_loc, _ } as token), ts')
    -> (
      let front = ts' () in
      match front with
      | Stream.Head ({ v = Token.Rbrack; span = sn, _, end_loc }, ts'') ->
          advance_type ts''
            { v = Ast.MtList t; span = (sn, start_loc, end_loc) }
      | _ ->
          let old_ts' = fun () -> front in
          let old_ts = fun () -> Stream.Head (token, old_ts') in
          (old_ts, t))
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, t)

and parse_types ts =
  match parse_type ts with
  | ts', None -> (ts', [])
  | ts', Some t -> (
      let front = ts' () in
      match front with
      | Stream.Head ({ v = Token.Comma }, ts'') ->
          let ts''', types = parse_types ts'' in
          (ts''', t :: types)
      | _ ->
          let old_ts' = fun () -> front in
          (old_ts', [ t ]))

let bind_type e x =
  bind_expect
    (fun ts ->
      match parse_type ts with ts', None -> None | ts', Some t -> Some (ts', t))
    e x

let rec parse_param (ts : Token.t Stream.t) :
    Token.t Stream.t * Ast.param option =
  let front = ts () in
  match front with
  | Stream.Head
      (({ v = Token.Name name; span = sn, start_loc, _ } as token), ts') -> (
      match parse_type ts' with
      | ts'', Some mt ->
          let sn, _, end_loc = mt.span in
          (ts'', Some { v = (name, mt); span = (sn, start_loc, end_loc) })
      | ts'', None ->
          let old_ts = fun () -> Stream.Head (token, ts'') in
          (old_ts, None))
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, None)

let rec parse_params ts =
  match parse_param ts with
  | ts', None -> (ts', [])
  | ts', Some param -> (
      let front = ts' () in
      match front with
      | Stream.Head ({ v = Token.Comma }, ts'') ->
          let ts''', ps = parse_params ts'' in
          (ts''', param :: ps)
      | _ ->
          let old_ts' = fun () -> front in
          (old_ts', [ param ]))

let prefix_bp = 13

let is_prefix op =
  match op with Token.Sub | Token.Add | Token.Not -> true | _ -> false

let prefix_combine ({ v = op; span = sn, start_loc, _ } : Token.t)
    ({ span = sn, _, end_loc } as r : Ast.expr) =
  let v =
    match op with
    | Token.Sub -> Some (Ast.Neg r)
    | Token.Add -> Some (Ast.Pos r)
    | Token.Not -> Some (Ast.Not r)
    | _ -> None
  in
  Option.bind v (fun v ->
      Some ({ v; span = (sn, start_loc, end_loc) } : Ast.expr))

let bp op =
  match op with
  | Token.Or -> Some (1, 2)
  | Token.Xor -> Some (3, 4)
  | Token.And -> Some (5, 6)
  | Token.Eq | Token.Gt | Token.Lt | Token.Neq | Token.Ge | Token.Le ->
      Some (7, 8)
  | Token.Add | Token.Sub -> Some (9, 10)
  | Token.Mul | Token.Div | Token.Mod -> Some (11, 12)
  | _ -> None

let combine ({ v = _; span = sn, start_loc, _ } as l : Ast.expr)
    ({ v = op } : Token.t) ({ v = _; span = sn, _, end_loc } as r : Ast.expr) =
  let v =
    match op with
    | Token.Or -> Some (Ast.Or (l, r))
    | Token.Xor -> Some (Ast.Xor (l, r))
    | Token.And -> Some (Ast.And (l, r))
    | Token.Eq -> Some (Ast.Eq (l, r))
    | Token.Gt -> Some (Ast.Gt (l, r))
    | Token.Lt -> Some (Ast.Lt (l, r))
    | Token.Neq -> Some (Ast.Neq (l, r))
    | Token.Ge -> Some (Ast.Ge (l, r))
    | Token.Le -> Some (Ast.Le (l, r))
    | Token.Add -> Some (Ast.Add (l, r))
    | Token.Sub -> Some (Ast.Sub (l, r))
    | Token.Mul -> Some (Ast.Mul (l, r))
    | Token.Div -> Some (Ast.Div (l, r))
    | Token.Mod -> Some (Ast.Mod (l, r))
    | _ -> None
  in
  Option.bind v (fun v ->
      Some ({ v; span = (sn, start_loc, end_loc) } : Ast.expr))

let rec bind_expr min_bp =
  bind_expect (fun ts ->
      match parse_expr ts min_bp with
      | ts', Some expr -> Some (ts', expr)
      | _ -> None)

and parse_expr (ts : Token.t Stream.t) min_bp =
  let front = ts () in
  match front with
  | Stream.End ->
      let old_ts = fun () -> front in
      (old_ts, None)
  | Stream.Head (({ v; span } as token), ts1) -> (
      match v with
      | Token.Int n ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.Int n; span } in
          (ts'', Some expr)
      | Token.Float n ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.Float n; span } in
          (ts'', Some expr)
      | Token.Char c ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.Char c; span } in
          (ts'', Some expr)
      | Token.Str s ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.Str s; span } in
          (ts'', Some expr)
      | Token.Name n ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.Name n; span } in
          (ts'', Some expr)
      | Token.True ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.True; span } in
          (ts'', Some expr)
      | Token.False ->
          let ts'', expr = advance_expr ts1 min_bp { v = Ast.False; span } in
          (ts'', Some expr)
      | Token.Lparen -> (
          let front' = ts1 () in
          match front' with
          | Stream.Head ({ v = Token.Rparen; span = sn, _, end_loc }, ts'') ->
              let sn, start_loc, _ = span in
              let ts''', expr =
                advance_expr ts'' min_bp
                  { v = Ast.Void; span = (sn, start_loc, end_loc) }
              in
              (ts''', Some expr)
          | _ ->
              let old_ts' = fun () -> front' in
              let ts'', inner =
                bind_expr 0 { v = "following expression"; span } old_ts'
              in
              let ts''', span2 =
                bind_x Token.Rparen { v = "closing )"; span } ts''
              in
              let sn, start_loc, _ = span in
              let sn, _, end_loc = span2 in
              let ts'''', expr =
                advance_expr ts''' min_bp
                  { v = inner.v; span = (sn, start_loc, end_loc) }
              in
              (ts'''', Some expr))
      | Token.Lbrack ->
          let ts'', es = parse_expr_list ts1 in
          let ts''', (sn, _, end_loc) =
            bind_x Token.Rbrack { v = "closing bracket"; span } ts''
          in
          let sn, start_loc, _ = span in
          let ts'''', expr =
            advance_expr ts''' min_bp
              { v = Ast.List es; span = (sn, start_loc, end_loc) }
          in
          (ts'''', Some expr)
      | Token.Fn ->
          let ts2, lspan =
            bind_x Token.Lparen { v = "param type spec"; span } ts1
          in
          let ts3, ps = parse_params ts2 in
          let ts4, _ =
            bind_x Token.Rparen { v = "matching )"; span = lspan } ts3
          in
          let ts5, t = bind_type { v = "result type spec"; span } ts4 in
          let ts6, (body : Ast.expr) =
            bind_expr 0 ({ v = "body"; span } : Errors.error) ts5
          in
          let sn, start_loc, _ = span in
          let sn, _, end_loc = body.span in
          let ts7, expr =
            advance_expr ts6 min_bp
              { v = Ast.FnVal (ps, t, body); span = (sn, start_loc, end_loc) }
          in
          (ts7, Some expr)
      | Token.Let ->
          let sn, start_loc, end_loc = span in
          let ts2, name =
            bind_name { v = "name"; span = (sn, start_loc, end_loc) } ts1
          in
          let ts3, ({ span = sn, _, end_loc; _ } as expr : Ast.expr) =
            bind_expr 0
              { v = "expression"; span = (sn, start_loc, end_loc) }
              ts2
          in
          let ts4, expr =
            advance_expr ts3 min_bp
              { v = Ast.Let (name, expr); span = (sn, start_loc, end_loc) }
          in
          (ts4, Some expr)
      | Token.If ->
          let sn, start_loc, end_loc = span in
          let ts2, expr =
            bind_expr 0
              { v = "expression"; span = (sn, start_loc, end_loc) }
              ts1
          in
          let ts3, ({ span = sn, _, end_loc } as body : Ast.expr) =
            bind_expr 0 { v = "body"; span = (sn, start_loc, end_loc) } ts2
          in
          let ts4, (sn, _, end_loc) =
            (bind_x Token.Else)
              { v = "body"; span = (sn, start_loc, end_loc) }
              ts3
          in
          let ts5, ({ span = sn, _, end_loc } as body2 : Ast.expr) =
            bind_expr 0 { v = "body"; span = (sn, start_loc, end_loc) } ts4
          in
          let ts6, expr =
            advance_expr ts5 min_bp
              {
                v = Ast.If (expr, body, body2);
                span = (sn, start_loc, end_loc);
              }
          in
          (ts6, Some expr)
      | Token.Lbrace ->
          let sn, start_loc, end_loc = span in
          let body = Queue.create () in
          let ts2 = parse_expr_seq span ts1 body in
          let ts3, (sn, _, end_loc) =
            (bind_x Token.Rbrace)
              { v = "'}'"; span = (sn, start_loc, end_loc) }
              ts2
          in
          let ts4, expr =
            advance_expr ts3 min_bp
              { v = Ast.Block body; span = (sn, start_loc, end_loc) }
          in
          (ts4, Some expr)
      | v when is_prefix v -> (
          let ts'', inner =
            bind_expr prefix_bp { v = "following expression"; span } ts1
          in
          let inner' = prefix_combine token inner in
          match inner' with
          | None -> assert false
          | Some inner' ->
              let ts''', expr = advance_expr ts'' min_bp inner' in
              (ts''', Some expr))
      | _ -> ((fun () -> front), None))

and advance_expr ts min_bp (left : Ast.expr) : Token.t Stream.t * Ast.expr =
  let front = ts () in
  match front with
  | Stream.End ->
      let old_ts = fun () -> front in
      (old_ts, left)
  | Stream.Head ({ v = Token.Lbrack; span }, ts') ->
      let ts'', inner = bind_expr 0 { v = "following expression"; span } ts' in
      let ts''', (sn, _, end_loc) =
        bind_x Token.Rbrack { v = "matching ]"; span } ts''
      in
      let sn, start_loc, _ = left.span in
      advance_expr ts''' min_bp
        { v = Ast.At (left, inner); span = (sn, start_loc, end_loc) }
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let ts'', es = parse_expr_list ts' in
      let ts''', (sn, _, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let sn, start_loc, _ = span in
      advance_expr ts''' min_bp
        { v = Ast.FnCall (left, es); span = (sn, start_loc, end_loc) }
  | Stream.Head (({ v; span } as token), ts') -> (
      match bp v with
      | Some (l, r) when min_bp < l -> (
          let ts'', right = bind_expr r { v = "right expression"; span } ts' in
          match combine left token right with
          | None -> assert false
          | Some left' -> advance_expr ts'' min_bp left')
      | _ ->
          let old_ts = fun () -> front in
          (old_ts, left))

and parse_expr_list ts =
  match parse_expr ts 0 with
  | ts', None -> (ts', [])
  | ts', Some expr -> (
      let front = ts' () in
      match front with
      | Stream.Head ({ v = Token.Comma }, ts'') ->
          let ts''', es = parse_expr_list ts'' in
          (ts''', expr :: es)
      | _ ->
          let old_ts' = fun () -> front in
          (old_ts', [ expr ]))

and parse_expr_seq span ts acc =
  match parse_expr ts 0 with
  | ts', None -> raise (Errors.Expected { v = "expression"; span })
  | ts', Some expr -> (
      let front = ts' () in
      match front with
      | Stream.Head ({ v = Token.Semicolon; span }, ts'') ->
          Queue.add expr acc;
          let ts''' = parse_expr_seq span ts'' acc in
          ts'''
      | _ ->
          Queue.add expr acc;
          let old_ts' = fun () -> front in
          old_ts')

let parse ts sn =
  let ds = Queue.create () in
  let ts' = parse_expr_seq (sn, (0, 0), (0, 0)) ts ds in
  match ts' () with
  | Stream.End -> ds
  | Stream.Head ({ span }, _) -> raise (Errors.Unexpected { v = "token"; span })
