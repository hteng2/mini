open Minic_lib

let rec scan_imports (ts : Token.token Stream.t) =
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
  bind_expect (fun (ts : Token.token Stream.t) ->
      match ts () with
      | Stream.Head ({ v; span }, ts') when v = x -> Some (ts', span)
      | _ -> None)

let bind_name e x =
  bind_expect
    (fun (ts : Token.token Stream.t) ->
      match ts () with
      | Stream.Head ({ v = Token.Name name }, ts') -> Some (ts', name)
      | _ -> None)
    e x

(* parse_fn must recover the token stream in the fail case *)
let rec parse_mult ts
    (parse_fn : Token.token Stream.t -> Token.token Stream.t * 'a option)
    (sep : Token.t) : Token.token Stream.t * 'a list =
  let rec helper ts acc =
    match parse_fn ts with
    | ts', None -> (ts', acc)
    | ts', Some x -> (
        match ts' () with
        | Stream.Head ({ v; span }, ts'') when v = sep -> helper ts'' (x :: acc)
        | front ->
            let old_ts' = fun () -> front in
            (old_ts', x :: acc))
  in
  let ts', acc = helper ts [] in
  (ts', List.rev acc)

let rec parse_type (ts : Token.token Stream.t) :
    Token.token Stream.t * Ast.parsed_type option =
  let front = ts () in
  match front with
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let ts'', (types : Ast.parsed_type list) =
        parse_mult ts' parse_type Token.Comma
      in
      let ts''', (sn, _, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let sn, start_loc, _ = span in
      let t = Ast.PtTup types in
      let ts'''', t =
        advance_type ts''' { v = t; span = (sn, start_loc, end_loc) }
      in
      (ts'''', Some t)
  | Stream.Head ({ v = Token.Name name; span }, ts') ->
      let ts'', t = advance_type ts' { v = Ast.PtBase name; span } in
      (ts'', Some t)
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, None)

and advance_type ts (left : Ast.parsed_type) =
  let front = ts () in
  match front with
  | Stream.Head ({ v = Token.To; span }, ts') -> (
      match parse_type ts' with
      | ts'', None -> raise (Errors.Expected { v = "type"; span })
      | ts'', Some t' ->
          let sn, start_loc, _ = left.span in
          let _, _, end_loc = t'.span in
          advance_type ts''
            { v = Ast.PtFn (left, t'); span = (sn, start_loc, end_loc) })
  | Stream.Head (({ v = Token.Lbrack; span = sn, start_loc, _ } as token), ts')
    -> (
      let front = ts' () in
      match front with
      | Stream.Head ({ v = Token.Rbrack; span = sn, _, end_loc }, ts'') ->
          advance_type ts''
            { v = Ast.PtList left; span = (sn, start_loc, end_loc) }
      | _ ->
          let old_ts' = fun () -> front in
          let old_ts = fun () -> Stream.Head (token, old_ts') in
          (old_ts, left))
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, left)

and bind_type e x =
  bind_expect
    (fun ts ->
      match parse_type ts with ts', None -> None | ts', Some t -> Some (ts', t))
    e x

let parse_typedef (ts : Token.token Stream.t) span :
    Token.token Stream.t * Ast.typedef =
  let ts', name = bind_name { v = "name"; span } ts in
  let ts'', t = bind_type { v = "type"; span } ts' in
  (ts'', (name, t))

let rec parse_param (ts : Token.token Stream.t) :
    Token.token Stream.t * Ast.param option =
  let front = ts () in
  match front with
  | Stream.Head ({ v = Token.Name name; span }, ts') ->
      let sn, start_loc, _ = span in
      let ts'', mt = bind_type { v = "param type"; span } ts' in
      let _, _, end_loc = mt.span in
      ( ts'',
        Some { v = Ast.PrmLeaf (name, mt); span = (sn, start_loc, end_loc) } )
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let sn, start_loc, _ = span in
      let ts'', ps = parse_mult ts' parse_param Token.Comma in
      let ts''', (sn, _, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let v' =
        if List.length ps = 0 then Ast.PrmUnit
        else if List.length ps = 1 then (List.hd (ps : Ast.param list)).v
        else Ast.PrmTuple ps
      in
      (ts''', Some { v = v'; span = (sn, start_loc, end_loc) })
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, None)

let rec parse_pattern (ts : Token.token Stream.t) :
    Token.token Stream.t * Ast.pattern option =
  let front = ts () in
  match front with
  | Stream.Head ({ v = Token.Name name; span }, ts') ->
      (ts', Some (Ast.PtrnLeaf name))
  | Stream.Head ({ v = Token.Lparen; span }, ts') ->
      let ts'', ps = parse_mult ts' parse_pattern Token.Comma in
      let ts''', (sn, _, end_loc) =
        bind_x Token.Rparen { v = "matching )"; span } ts''
      in
      let v' =
        if List.length ps = 0 then Ast.PtrnUnit
        else if List.length ps = 1 then List.hd ps
        else Ast.PtrnTuple ps
      in
      (ts''', Some v')
  | _ ->
      let old_ts = fun () -> front in
      (old_ts, None)

let prefix_bp = 13

let is_prefix op =
  match op with Token.Sub | Token.Add | Token.Not -> true | _ -> false

let prefix_combine ({ v = op; span = sn, start_loc, _ } : Token.token)
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
    ({ v = op } : Token.token)
    ({ v = _; span = sn, _, end_loc } as r : Ast.expr) =
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

and parse_expr (ts : Token.token Stream.t) min_bp =
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
      | Token.Lparen ->
          let ts2, (es : Ast.expr list) =
            parse_mult ts1 (fun ts -> parse_expr ts 0) Token.Comma
          in
          let ts3, (sn, _, end_loc) =
            bind_x Token.Rparen { v = "matching )"; span } ts2
          in
          let sn, start_loc, _ = span in
          let e =
            if List.length es = 0 then Ast.Void
            else if List.length es = 1 then (List.hd es).v
            else Ast.Tuple es
          in
          let ts4, expr =
            advance_expr ts3 min_bp { v = e; span = (sn, start_loc, end_loc) }
          in
          (ts4, Some expr)
      | Token.Lbrack ->
          let ts2, es =
            parse_mult ts1 (fun ts -> parse_expr ts 0) Token.Comma
          in
          let ts3, (sn, _, end_loc) =
            bind_x Token.Rbrack { v = "closing bracket"; span } ts2
          in
          let sn, start_loc, _ = span in
          let ts4, expr =
            advance_expr ts3 min_bp
              { v = Ast.List es; span = (sn, start_loc, end_loc) }
          in
          (ts4, Some expr)
      | Token.Fn ->
          let ts1, typevars =
            match ts1 () with
            | Stream.Head ({ v = Token.Lbrack; span = lspan }, ts') ->
                let ts'', typevars =
                  parse_mult ts'
                    (fun (t : Token.token Stream.t) ->
                      match t () with
                      | Stream.Head ({ v = Token.Name n; span }, ts') ->
                          (ts', Some n)
                      | f -> ((fun () -> f), None))
                    Token.Comma
                in
                let ts''', _ =
                  bind_x Token.Rbrack { v = "matching ]"; span = lspan } ts''
                in
                (ts''', typevars)
            | front -> ((fun () -> front), [])
          in
          let ts2, param =
            match ts1 () with
            | Stream.Head ({ v = Token.Lparen; span = lspan }, _) as front -> (
                let old_ts1 = fun () -> front in
                let ts2, param = parse_param old_ts1 in
                match param with
                | None ->
                    raise (Errors.Expected { v = "param type spec"; span })
                | Some p -> (ts2, p))
            | _ -> raise (Errors.Expected { v = "param type spec"; span })
          in
          let ts3, t = bind_type { v = "result type spec"; span } ts2 in
          let ts4, (body : Ast.expr) =
            bind_expr 0 ({ v = "body"; span } : Errors.error) ts3
          in
          let sn, start_loc, _ = span in
          let sn, _, end_loc = body.span in
          let ts5, expr =
            advance_expr ts4 min_bp
              {
                v = Ast.FnVal (typevars, param, t, body);
                span = (sn, start_loc, end_loc);
              }
          in
          (ts5, Some expr)
      | Token.Bind ->
          let sn, start_loc, end_loc = span in
          let ts2, ptrn =
            match parse_pattern ts1 with
            | _, None -> raise (Errors.Expected { v = "pattern"; span })
            | ts2, Some ptrn -> (ts2, ptrn)
          in
          let ts3, ({ span = sn, _, end_loc; _ } as expr : Ast.expr) =
            bind_expr 0
              { v = "expression"; span = (sn, start_loc, end_loc) }
              ts2
          in
          let ts4, expr =
            advance_expr ts3 min_bp
              { v = Ast.Bind (ptrn, expr); span = (sn, start_loc, end_loc) }
          in
          (ts4, Some expr)
      | Token.If ->
          let sn, start_loc, end_loc = span in
          let ts2, (_, _, end_loc) =
            bind_x Token.Lparen
              { v = "condition"; span = (sn, start_loc, end_loc) }
              ts1
          in
          let ts3, expr =
            bind_expr 0
              { v = "expression"; span = (sn, start_loc, end_loc) }
              ts2
          in
          let _, _, end_loc = expr.span in
          let ts4, (_, _, end_loc) =
            bind_x Token.Rparen
              { v = "matching )"; span = (sn, start_loc, end_loc) }
              ts3
          in
          let ts5, ({ span = sn, _, end_loc } as body : Ast.expr) =
            bind_expr 0
              { v = "expression"; span = (sn, start_loc, end_loc) }
              ts4
          in
          let ts6, (sn, _, end_loc) =
            (bind_x Token.Else)
              { v = "else"; span = (sn, start_loc, end_loc) }
              ts5
          in
          let ts7, ({ span = sn, _, end_loc } as body2 : Ast.expr) =
            bind_expr 0
              { v = "expression"; span = (sn, start_loc, end_loc) }
              ts6
          in
          let ts8, expr =
            advance_expr ts7 min_bp
              {
                v = Ast.If (expr, body, body2);
                span = (sn, start_loc, end_loc);
              }
          in
          (ts8, Some expr)
      | Token.Lbrace ->
          let sn, start_loc, end_loc = span in
          let ts2, head, tail = parse_expr_seq span ts1 in
          let ts3, (sn, _, end_loc) =
            (bind_x Token.Rbrace)
              { v = "'}'"; span = (sn, start_loc, end_loc) }
              ts2
          in
          let ts4, expr =
            advance_expr ts3 min_bp
              { v = Ast.Block (head, tail); span = (sn, start_loc, end_loc) }
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

and advance_expr ts min_bp (left : Ast.expr) : Token.token Stream.t * Ast.expr =
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
  | Stream.Head (({ v; span } as token), ts') -> (
      match bp v with
      | Some (l, r) -> (
          if min_bp >= l then
            let old_ts = fun () -> front in
            (old_ts, left)
          else
            let ts'', right =
              bind_expr r { v = "right expression"; span } ts'
            in
            match combine left token right with
            | None -> assert false
            | Some left' -> advance_expr ts'' min_bp left')
      | _ ->
          let old_ts = fun () -> front in
          if min_bp < prefix_bp then
            match parse_expr old_ts prefix_bp with
            | old_ts2, None -> (old_ts2, left)
            | ts'', Some expr ->
                let sn, start_loc, _ = left.span in
                let _, _, end_loc = expr.span in
                advance_expr ts'' min_bp
                  {
                    v = Ast.FnCall (left, expr);
                    span = (sn, start_loc, end_loc);
                  }
          else (old_ts, left))

and parse_expr_seq span ts : Token.token Stream.t * Ast.stmt Queue.t * Ast.expr
    =
  let head = Queue.create () in
  let rec helper span (ts : Token.token Stream.t) =
    match ts () with
    | Stream.Head ({ v = Token.Type; span }, ts') ->
        let ts'', td = parse_typedef ts' span in
        let ts''', _ = bind_x Token.Semicolon { v = ";"; span } ts'' in
        Queue.add (Ast.Typedef td) head;
        helper span ts'''
    | front -> (
        let old_ts = fun () -> front in
        match parse_expr old_ts 0 with
        | ts', None -> raise (Errors.Expected { v = "expression"; span })
        | ts', Some expr -> (
            match ts' () with
            | Stream.Head ({ v = Token.Semicolon; span }, ts'') ->
                Queue.add (Ast.Expr expr) head;
                helper span ts''
            | front ->
                let old_ts' = fun () -> front in
                (old_ts', expr)))
  in
  let ts', tail = helper span ts in
  (ts', head, tail)

let parse ts sn =
  let ts', head, tail = parse_expr_seq (sn, (0, 0), (0, 0)) ts in
  match ts' () with
  | Stream.End -> (head, tail)
  | Stream.Head ({ span }, _) -> raise (Errors.Unexpected { v = "token"; span })
