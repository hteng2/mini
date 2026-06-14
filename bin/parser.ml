let bind_or f g x = match f x with Some y -> y | _ -> g ()
let bind_expect f e = f (fun () -> raise (Errors.Expected e))

let bind_x x =
  bind_or (fun (ts : Token.t list) ->
      match ts with { v = x; span } :: ts' -> Some (ts', span) | _ -> None)

let rec bind_name g x =
  bind_or
    (fun ts ->
      match ts with
      | ({ v = Token.Name name } : Token.t) :: ts' -> Some (ts', name)
      | _ -> None)
    g x

let rec parse_type (ts : Token.t list) : (Token.t list * Ast.mini_type) option =
  match ts with
  | { v = Token.Lparen; span } :: ts' ->
      Option.bind (parse_type ts') (fun (ts'', { v }) ->
          let ts''', (_, end_loc) =
            bind_expect (bind_x Token.Rparen) { v = "matching )"; span } ts''
          in
          let start_loc, _ = span in

          Some (ts'', ({ v; span = (start_loc, end_loc) } : Ast.mini_type)))
  | { v = Token.Name name; span } :: ts' ->
      Some (advance ts' ({ v = Ast.MtBase name; span } : Ast.mini_type))
  | _ -> None

and advance (ts : Token.t list) (t : Ast.mini_type) :
    Token.t list * Ast.mini_type =
  match ts with
  | { v = Token.Lbrack } :: { v = Token.Rbrack; span = _, end_loc } :: ts' ->
      let start_loc, _ = t.span in
      advance ts' { v = Ast.MtList t; span = (start_loc, end_loc) }
  | { v = Token.Lparen; span } :: ts' ->
      let ts'', types = parse_types ts' [] in
      let ts''', (_, end_loc) =
        bind_expect (bind_x Token.Rparen) { v = "matching )"; span } ts''
      in
      let start_loc, _ = span in
      advance ts''' { v = Ast.MtFn (t, types); span = (start_loc, end_loc) }
  | _ -> (ts, t)

and parse_types (ts : Token.t list) (acc : Ast.mini_type list) =
  match parse_type ts with
  | None -> (ts, List.rev acc)
  | Some (ts', t) -> (
      match ts' with
      | { v = Token.Comma } :: ts'' -> parse_types ts'' (t :: acc)
      | _ -> (ts', List.rev (t :: acc)))

let rec parse_param (ts : Token.t list) : (Token.t list * Ast.param) option =
  match ts with
  | ({ v = Token.Name name; span = start_loc, _ } : Token.t) :: ts' -> (
      match parse_type ts' with
      | Some (ts'', mt) ->
          let _, end_loc = mt.span in
          Some (ts'', { v = (name, mt); span = (start_loc, end_loc) })
      | None -> None)
  | _ -> None

let rec parse_params (ts : Token.t list) (acc : Ast.param list) :
    Token.t list * Ast.param list =
  match parse_param ts with
  | None -> (ts, List.rev acc)
  | Some (ts', param) -> (
      match ts' with
      | { v = Token.Comma } :: ts'' -> parse_params ts (param :: acc)
      | _ -> (ts', List.rev (param :: acc)))

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

let rec bind_expr min_bp g ts = bind_or (fun ts -> parse_expr ts min_bp) g ts

and parse_expr (ts : Token.t list) (min_bp : int) :
    (Token.t list * Ast.expr) option =
  match ts with
  | [] -> None
  | ({ v; span } as token) :: ts' -> (
      match v with
      | Token.Num n -> Some (advance_expr ts' min_bp { v = Ast.Num n; span })
      | Token.Name n -> Some (advance_expr ts' min_bp { v = Ast.Name n; span })
      | Token.True -> Some (advance_expr ts' min_bp { v = Ast.True; span })
      | Token.False -> Some (advance_expr ts' min_bp { v = Ast.False; span })
      | Token.Void -> Some (advance_expr ts' min_bp { v = Ast.Void; span })
      | Token.Lparen ->
          let ts'', (inner : Ast.expr) =
            bind_expect (bind_expr 0) { v = "following expression"; span } ts'
          in
          let ts''', span2 =
            bind_expect (bind_x Token.Rparen) { v = "closing )"; span } ts''
          in
          let start_loc, _ = span in
          let _, end_loc = span2 in
          Some
            (advance_expr ts''' min_bp
               { v = inner.v; span = (start_loc, end_loc) })
      | Token.Lbrack ->
          let ts'', es = parse_exprs ts' [] in
          let ts''', (_, end_loc) =
            bind_expect (bind_x Token.Rbrack)
              { v = "closing bracket"; span }
              ts''
          in
          let start_loc, _ = span in
          Some
            (advance_expr ts''' min_bp
               { v = Ast.List es; span = (start_loc, end_loc) })
      | Token.Fn ->
          let ts2, lspan =
            bind_expect (bind_x Token.Lparen)
              { v = "param type spec"; span }
              ts'
          in
          let ts3, ps = parse_params ts2 [] in
          let ts4, _ =
            bind_expect (bind_x Token.Rparen)
              { v = "matching )"; span = lspan }
              ts3
          in
          let ts5, t =
            bind_expect (bind_or parse_type)
              { v = "result type spec"; span }
              ts4
          in
          let ts6, body =
            bind_expect (bind_or parse_dec) { v = "body"; span } ts5
          in
          let start_loc, _ = span in
          let _, end_loc = body.span in
          Some
            (advance_expr ts6 min_bp
               { v = Ast.FnVal (ps, t, body); span = (start_loc, end_loc) })
      | v when is_prefix v -> (
          let ts'', inner =
            bind_expect (bind_expr prefix_bp)
              { v = "following expression"; span }
              ts'
          in
          let inner' = prefix_combine token inner in
          match inner' with
          | None -> assert false
          | Some inner' -> Some (advance_expr ts'' min_bp inner'))
      | _ -> None)

and advance_expr (ts : Token.t list) (min_bp : int) (left : Ast.expr) :
    Token.t list * Ast.expr =
  match ts with
  | [] -> (ts, left)
  | { v = Token.Lbrack; span } :: ts' ->
      let ts'', inner =
        bind_expect (bind_expr 0) { v = "following expression"; span } ts'
      in
      let ts''', (_, end_loc) =
        bind_expect (bind_x Token.Rbrack) { v = "matching ]"; span } ts''
      in
      let start_loc, _ = left.span in
      advance_expr ts''' min_bp
        { v = Ast.At (left, inner); span = (start_loc, end_loc) }
  | { v = Token.Lparen; span } :: ts' ->
      let ts'', es = parse_exprs ts' [] in
      let ts''', (_, end_loc) =
        bind_expect (bind_x Token.Rparen) { v = "matching )"; span } ts''
      in
      let start_loc, _ = span in
      advance_expr ts''' min_bp
        { v = Ast.FnCall (left, es); span = (start_loc, end_loc) }
  | ({ v; span } as token) :: ts' -> (
      match bp v with
      | Some (l, r) when min_bp < l -> (
          let ts'', right =
            bind_expect (bind_expr r) { v = "right expression"; span } ts'
          in
          match combine left token right with
          | None -> assert false
          | Some left' -> advance_expr ts'' min_bp left')
      | _ -> (ts, left))

and parse_exprs (ts : Token.t list) (acc : Ast.expr list) =
  match parse_expr ts 0 with
  | None -> (ts, List.rev acc)
  | Some (ts', expr) -> (
      match ts' with
      | { v = Token.Comma } :: ts'' -> parse_exprs ts'' (expr :: acc)
      | _ -> (ts', List.rev (expr :: acc)))

and bind_id g x = bind_or parse_id g x

and parse_id (ts : Token.t list) : (Token.t list * Ast.identifier) option =
  match ts with
  | { v = Token.Name name; span } :: ts' ->
      Some (advance_id ts' { v = Ast.IdName name; span })
  | _ -> None

and advance_id (ts : Token.t list) (left : Ast.identifier) :
    Token.t list * Ast.identifier =
  match ts with
  | { v = Token.Lbrack; span } :: ts' ->
      let ts'', inner =
        bind_expect (bind_expr 0) { v = "following expression"; span } ts'
      in
      let ts''', (_, end_loc) =
        bind_expect (bind_x Token.Rbrack) { v = "closing bracket"; span } ts''
      in
      let start_loc, _ = left.span in
      advance_id ts'''
        { v = Ast.IdAt (left, inner); span = (start_loc, end_loc) }
  | _ -> (ts, left)

and parse_dec (ts : Token.t list) : (Token.t list * Ast.dec) option =
  match ts with
  | { v = Token.Let; span = start_loc, end_loc } :: ts1 ->
      let ts2, name =
        bind_expect bind_name { v = "name"; span = (start_loc, end_loc) } ts1
      in
      let ts3, (_, end_loc) =
        bind_expect (bind_x Token.Eq)
          { v = "'='"; span = (start_loc, end_loc) }
          ts2
      in
      let ts4, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts3
      in
      Some (ts4, { v = Ast.Let (name, expr); span = (start_loc, end_loc) })
  | { v = Token.Var; span = start_loc, end_loc } :: ts1 ->
      let ts2, name =
        bind_expect bind_name { v = "name"; span = (start_loc, end_loc) } ts1
      in
      let ts3, (_, end_loc) =
        bind_expect (bind_x Token.Eq)
          { v = "'='"; span = (start_loc, end_loc) }
          ts2
      in
      let ts4, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts3
      in
      Some (ts4, { v = Ast.Var (name, expr); span = (start_loc, end_loc) })
  | { v = Token.Name name; span = start_loc, end_loc } :: ts1 ->
      let ts1, id =
        bind_expect bind_id { v = "identifier"; span = (start_loc, end_loc) } ts
      in
      let ts2, (_, end_loc) =
        bind_expect (bind_x Token.Eq)
          { v = "'='"; span = (start_loc, end_loc) }
          ts1
      in
      let ts3, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts2
      in
      Some (ts3, { v = Ast.VarSet (id, expr); span = (start_loc, end_loc) })
  | { v = Token.Print; span = start_loc, end_loc } :: ts1 ->
      let ts2, expr =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      Some (ts2, { v = Ast.Print expr; span = (start_loc, end_loc) })
  | { v = Token.Println; span = start_loc, end_loc } :: ts1 ->
      let ts2, expr =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      Some (ts2, { v = Ast.Println expr; span = (start_loc, end_loc) })
  | { v = Token.If; span = start_loc, end_loc } :: ts1 -> (
      let ts2, expr =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      let ts3, ({ span = _, end_loc } as body : Ast.dec) =
        bind_expect (bind_or parse_dec)
          { v = "body"; span = (start_loc, end_loc) }
          ts2
      in
      match ts3 with
      | { v = Token.Else; span = _, end_loc; _ } :: ts4 ->
          let ts5, ({ span = _, end_loc } as body2 : Ast.dec) =
            bind_expect (bind_or parse_dec)
              { v = "body"; span = (start_loc, end_loc) }
              ts4
          in
          Some
            ( ts5,
              {
                v = Ast.If (expr, body, Some body2);
                span = (start_loc, end_loc);
              } )
      | _ ->
          Some
            (ts3, { v = Ast.If (expr, body, None); span = (start_loc, end_loc) })
      )
  | { v = Token.While; span = start_loc, end_loc } :: ts1 ->
      let ts2, expr =
        bind_expect (bind_expr 0)
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      let ts3, ({ span = _, end_loc } as body : Ast.dec) =
        bind_expect (bind_or parse_dec)
          { v = "body"; span = (start_loc, end_loc) }
          ts2
      in
      Some (ts3, { v = Ast.While (expr, body); span = (start_loc, end_loc) })
  | { v = Token.Break; span } :: ts' -> Some (ts', { v = Ast.Break; span })
  | { v = Token.Continue; span } :: ts' -> Some (ts', { v = Ast.Continue; span })
  | { v = Token.Return; span } :: ts1 ->
      let ts2, expr =
        bind_expect (bind_expr 0) { v = "expression"; span } ts1
      in
      let start_loc, _ = span in
      let _, end_loc = expr.span in
      Some (ts2, { v = Ast.Return expr; span = (start_loc, end_loc) })
  | { v = Token.Lbrace; span = start_loc, end_loc } :: ts1 ->
      let ts2, body = p ts1 [] in
      let ts3, _ =
        bind_expect (bind_x Token.Rbrace)
          { v = "'}'"; span = (start_loc, end_loc) }
          ts2
      in
      Some (ts3, { v = Ast.Block body; span = (start_loc, end_loc) })
  | t :: _ -> raise (Errors.Unexpected { v = "token"; span = t.span })
  | _ -> None

and p (ts : Token.t list) (ds_acc : Ast.dec list) : Token.t list * Ast.dec list
    =
  match ts with
  | [] -> ([], List.rev ds_acc)
  | { v = Token.Rbrace } :: _ -> (ts, List.rev ds_acc)
  | { span } :: _ ->
      let ts', d =
        bind_expect (bind_or parse_dec) { v = "declaration"; span } ts
      in
      p ts' (d :: ds_acc)

let parse ts =
  match p ts [] with
  | [], ds -> ds
  | t :: _, _ -> raise (Errors.Unexpected { v = "token"; span = t.span })
