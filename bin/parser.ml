let prefix_bp = 13

exception EndToken

let is_op t =
  match t with
  | Token.Eq | Token.Gt | Token.Lt | Token.Add | Token.Sub | Token.Mul
  | Token.Div | Token.And | Token.Or | Token.Not | Token.Xor ->
      true
  | _ -> false

let prefix_combine (op, r) =
  match op with
  | Token.Sub -> Ast.Neg r
  | Token.Add -> Ast.Pos r
  | Token.Not -> Ast.Not r
  | _ -> raise (Failure "unrecognized prefix operator")

let bp op =
  match op with
  | Token.Or -> (1, 2)
  | Token.Xor -> (3, 4)
  | Token.And -> (5, 6)
  | Token.Eq -> (8, 7)
  | Token.Add -> (9, 10)
  | Token.Sub -> (9, 10)
  | Token.Mul -> (11, 12)
  | Token.Div -> (11, 12)
  | _ -> raise (Failure "unrecognized infix operator")

let combine (l, op, r) =
  match op with
  | Token.Or -> Ast.Or (l, r)
  | Token.Xor -> Ast.Xor (l, r)
  | Token.And -> Ast.And (l, r)
  | Token.Eq -> Ast.Eq (l, r)
  | Token.Add -> Ast.Add (l, r)
  | Token.Sub -> Ast.Sub (l, r)
  | Token.Mul -> Ast.Mul (l, r)
  | Token.Div -> Ast.Div (l, r)
  | _ -> raise (Failure "unrecognized infix operator")

let rec parse_expr (ts : Token.token list) (min_bp : int)
    (k : Token.token list * Ast.expr -> 'a) =
  match ts with
  | Token.Num n :: ts' -> advance ts' min_bp (Ast.Num n) k
  | Token.Name n :: ts' -> advance ts' min_bp (Ast.Name n) k
  | Token.True :: ts' -> advance ts' min_bp Ast.True k
  | Token.False :: ts' -> advance ts' min_bp Ast.False k
  | Token.Lparen :: ts' ->
      parse_expr ts' 0 (fun (ts'', inner) ->
          match ts'' with
          | Rparen :: ts''' -> advance ts''' min_bp inner k
          | _ -> raise (Failure "expected Rparen"))
  | Token.Rparen :: ts' -> raise (Failure "unexpected Rparen")
  (* at this point, only operators remain *)
  | t :: ts' when is_op t ->
      parse_expr ts' prefix_bp (fun (ts'', inner) ->
          advance ts'' min_bp (prefix_combine (t, inner)) k)
  | _ -> raise (Failure "expected expression")

and advance (ts : Token.token list) (min_bp : int) (left : Ast.expr)
    (k : Token.token list * Ast.expr -> 'a) =
  match ts with
  | [] -> k ([], left)
  | t :: ts' when is_op t ->
      let l, r = bp t in
      if min_bp >= l then k (ts, left)
      else
        parse_expr ts' r (fun (ts'', right) ->
            advance ts'' min_bp (combine (left, t, right)) k)
  | _ -> k (ts, left)

let rec parse_dec (ts : Token.token list) : Token.token list * Ast.expr Ast.dec
    =
  match ts with
  | [] -> raise (Failure "expected token")
  | Token.Let :: ts' ->
      parse_expr ts' 0 (fun (ts'', expr) ->
          match expr with
          | Ast.Eq (Name s, e) -> (ts'', Ast.Let (s, e))
          | _ -> raise (Failure "unexpected equality"))
  | Token.Var :: ts' ->
      parse_expr ts' 0 (fun (ts'', expr) ->
          match expr with
          | Ast.Eq (Name s, e) -> (ts'', Ast.Var (s, e))
          | _ -> raise (Failure "unexpected equality"))
  | Token.Name _ :: _ ->
      parse_expr ts 0 (fun (ts', expr) ->
          match expr with
          | Ast.Eq (Name s, e) -> (ts', Ast.VarSet (s, e))
          | _ -> raise (Failure "unexpected equality"))
  | Token.Print :: ts' ->
      parse_expr ts' 0 (fun (ts'', expr) -> (ts'', Ast.Print expr))
  | Token.If :: ts1 ->
      parse_expr ts1 0 (fun (ts2, expr) ->
          match ts2 with
          | Token.Then :: ts3 -> (
              let ts4, body = p ts3 [] in
              match ts4 with
              | Token.End :: ts5 -> (ts5, Ast.If (expr, body))
              | _ -> raise (Failure "expected \"end\""))
          | _ -> raise (Failure "expected \"then\""))
  | Token.End :: _ -> raise EndToken
  | _ -> raise (Failure "unexpected token")

and p (ts : Token.token list) (ds_acc : Ast.expr Ast.dec list) :
    Token.token list * Ast.expr Ast.dec list =
  match ts with
  | [] -> ([], List.rev ds_acc)
  | _ -> (
      try
        let ts', d = parse_dec ts in
        p ts' (d :: ds_acc)
      with EndToken -> (ts, List.rev ds_acc))

let parse ts =
  match p ts [] with [], ds -> ds | _ -> raise (Failure "extra end somewhere")
