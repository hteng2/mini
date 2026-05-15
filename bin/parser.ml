let prefix_bp = 13

exception EndToken of Token.t
exception Missing

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

let rec parse_expr (ts : Token.t list) (min_bp : int)
    (k : Token.t list * Ast.expr -> 'a) =
  match ts with
  | [] -> raise Missing
  | ({ v; span } as token) :: ts' -> (
      match v with
      | Token.Num n -> advance ts' min_bp { v = Ast.Num n; span } (fun x -> k x)
      | Token.Name n ->
          advance ts' min_bp { v = Ast.Name n; span } (fun x -> k x)
      | Token.True -> advance ts' min_bp { v = Ast.True; span } (fun x -> k x)
      | Token.False -> advance ts' min_bp { v = Ast.False; span } (fun x -> k x)
      | Token.Lparen ->
          parse_tuple ts' (fun (ts'', expr) ->
              advance ts'' min_bp expr (fun x -> k x))
      | _ -> (
          try
            parse_expr ts' prefix_bp (fun (ts'', inner) ->
                let inner' = prefix_combine token inner in
                match inner' with
                | None ->
                    raise (Errors.Expected { v = "prefix operator"; span })
                | Some inner' -> advance ts'' min_bp inner' (fun x -> k x))
          with Missing -> raise (Errors.Expected { v = "expression"; span })))

and advance (ts : Token.t list) (min_bp : int) (left : Ast.expr)
    (k : Token.t list * Ast.expr -> 'a) =
  match ts with
  | [] -> k (ts, left)
  | ({ v; span } as token) :: ts' -> (
      match bp v with
      | None -> k (ts, left)
      | Some (l, r) -> (
          if min_bp >= l then k (ts, left)
          else
            try
              parse_expr ts' r (fun (ts'', right) ->
                  match combine left token right with
                  | None -> assert false
                  | Some left' -> advance ts'' min_bp left' k)
            with Missing ->
              raise (Errors.Expected { v = "right expression"; span })))

and parse_tuple (ts : Token.t list) (k : Token.t list * Ast.expr -> 'a) =
  match ts with
  | { v = Token.Lparen; span = start_loc, _ } :: ts ->
      parse_expr_list ts (fun (ts', es) ->
          match ts' with
          | ({ v = Token.Rparen; span = _, end_loc } : Token.t) :: ts'' ->
              k (ts'', { v = Ast.Tuple es; span = (start_loc, end_loc) })
          | _ -> assert false)
  | _ -> assert false

and parse_expr_list (ts : Token.t list) (k : Token.t list * Ast.expr list -> 'a)
    =
  match ts with
  | { v = _; span } :: _ -> (
      try
        parse_expr ts 0 (fun (ts', expr) ->
            match ts' with
            | { v = Token.Rparen; _ } :: ts'' -> k (ts', expr :: [])
            | { v = Token.Comma; _ } :: ts'' ->
                parse_expr_list ts'' (fun (ts''', es) -> k (ts''', expr :: es))
            | _ -> raise (Errors.Unexpected { v = "token)"; span }))
      with Missing -> raise (Errors.Expected { v = "expression"; span }))
  | _ -> assert false

let bind_or f g = fun x -> match f x with Some y -> y | _ -> g ()

let bind_name =
  bind_or (fun (ts : Token.t list) ->
      match ts with
      | { v = Token.Name name; span } :: ts' -> Some (ts', name, span)
      | _ -> None)

let bind_x x =
  bind_or (fun (ts : Token.t list) ->
      match ts with { v = x; span } :: ts' -> Some (ts', x, span) | _ -> None)

let bind_expr =
  bind_or (fun ts ->
      try parse_expr ts 0 (fun x -> Some x) with Missing -> None)

let bind_expect f e = f (fun () -> raise (Errors.Expected e))

let rec parse_dec (ts : Token.t list) : Token.t list * Ast.dec =
  match ts with
  | { v = Token.Let; span = start_loc, end_loc } :: ts1 ->
      let ts2, name, (_, end_loc) =
        bind_expect bind_name { v = "name"; span = (start_loc, end_loc) } ts1
      in
      let ts3, _, (_, end_loc) =
        bind_expect (bind_x Token.Eq)
          { v = "'='"; span = (start_loc, end_loc) }
          ts2
      in
      let ts4, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        bind_expect bind_expr
          { v = "expression"; span = (start_loc, end_loc) }
          ts3
      in
      (ts4, { v = Ast.Let (name, expr); span = (start_loc, end_loc) })
  | { v = Token.Var; span = start_loc, end_loc } :: ts1 ->
      let ts2, name, (_, end_loc) =
        bind_expect bind_name { v = "name"; span = (start_loc, end_loc) } ts1
      in
      let ts3, _, (_, end_loc) =
        bind_expect (bind_x Token.Eq)
          { v = "'='"; span = (start_loc, end_loc) }
          ts2
      in
      let ts4, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        bind_expect bind_expr
          { v = "expression"; span = (start_loc, end_loc) }
          ts3
      in
      (ts4, { v = Ast.Var (name, expr); span = (start_loc, end_loc) })
  | { v = Token.Name name; span = start_loc, end_loc } :: ts1 ->
      let ts2, _, (_, end_loc) =
        bind_expect (bind_x Token.Eq)
          { v = "'='"; span = (start_loc, end_loc) }
          ts1
      in
      let ts3, ({ span = _, end_loc; _ } as expr : Ast.expr) =
        bind_expect bind_expr
          { v = "expression"; span = (start_loc, end_loc) }
          ts2
      in
      (ts3, { v = Ast.VarSet (name, expr); span = (start_loc, end_loc) })
  | { v = Token.Print; span = start_loc, end_loc } :: ts1 ->
      let ts2, expr =
        bind_expect bind_expr
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      (ts2, { v = Ast.Print expr; span = (start_loc, end_loc) })
  | { v = Token.If; span = start_loc, end_loc } :: ts1 -> (
      let ts2, expr =
        bind_expect bind_expr
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      let ts3, _, (_, end_loc) =
        bind_expect (bind_x Token.Then)
          { v = "'then'"; span = (start_loc, end_loc) }
          ts2
      in
      let ts4, body = p ts3 [] in
      match ts4 with
      | { v = Token.End; span = _, end_loc; _ } :: ts5 ->
          ( ts5,
            {
              v = Ast.If (Ast.IfThen (expr, body));
              span = (start_loc, end_loc);
            } )
      | { v = Token.Else; span = _, end_loc; _ } :: ts5 ->
          let ts6, body2 = p ts5 [] in
          let ts7, _, (_, end_loc) =
            bind_expect (bind_x Token.End)
              { v = "'end'"; span = (start_loc, end_loc) }
              ts6
          in
          ( ts7,
            {
              v = Ast.If (Ast.IfThenElse (expr, body, body2));
              span = (start_loc, end_loc);
            } )
      | _ ->
          raise (Errors.Expected { v = "'end'"; span = (start_loc, end_loc) }))
  | { v = Token.While; span = start_loc, end_loc } :: ts1 ->
      let ts2, expr =
        bind_expect bind_expr
          { v = "expression"; span = (start_loc, end_loc) }
          ts1
      in
      let ts3, _, (_, end_loc) =
        bind_expect (bind_x Token.Do)
          { v = "'do'"; span = (start_loc, end_loc) }
          ts2
      in
      let ts4, body = p ts3 [] in
      let ts5, _, (_, end_loc) =
        bind_expect (bind_x Token.Done)
          { v = "'done'"; span = (start_loc, end_loc) }
          ts4
      in
      (ts5, { v = Ast.While (expr, body); span = (start_loc, end_loc) })
  | ({ v = Token.Else; span = start_loc, end_loc } as token) :: _
  | ({ v = Token.End; span = start_loc, end_loc } as token) :: _
  | ({ v = Token.Done; span = start_loc, end_loc } as token) :: _ ->
      raise (EndToken token)
  | t :: _ -> raise (Errors.Unexpected { v = "token"; span = t.span })
  | _ -> assert false

and p (ts : Token.t list) (ds_acc : Ast.dec list) : Token.t list * Ast.dec list
    =
  match ts with
  | [] -> ([], List.rev ds_acc)
  | _ -> (
      try
        let ts', d = parse_dec ts in
        p ts' (d :: ds_acc)
      with EndToken _ -> (ts, List.rev ds_acc))

let parse ts =
  match p ts [] with [], ds -> ds | _ -> raise (Failure "extra end somewhere")
