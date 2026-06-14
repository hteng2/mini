let c2t c =
  match c with
  | '(' -> Some Token.Lparen
  | ')' -> Some Token.Rparen
  | '[' -> Some Token.Lbrack
  | ']' -> Some Token.Rbrack
  | '{' -> Some Token.Lbrace
  | '}' -> Some Token.Rbrace
  | ',' -> Some Token.Comma
  | '=' -> Some Token.Eq
  | '>' -> Some Token.Gt
  | '<' -> Some Token.Lt
  | '+' -> Some Token.Add
  | '-' -> Some Token.Sub
  | '*' -> Some Token.Mul
  | '/' -> Some Token.Div
  | '%' -> Some Token.Mod
  | '&' -> Some Token.And
  | '|' -> Some Token.Or
  | '!' -> Some Token.Not
  | '^' -> Some Token.Xor
  | '_' -> Some Token.Void
  | _ -> None

let n2t n =
  match n with
  | "let" -> Some Token.Let
  | "var" -> Some Token.Var
  | "print" -> Some Token.Print
  | "println" -> Some Token.Println
  | "true" -> Some Token.True
  | "false" -> Some Token.False
  | "if" -> Some Token.If
  | "else" -> Some Token.Else
  | "while" -> Some Token.While
  | "break" -> Some Token.Break
  | "continue" -> Some Token.Continue
  | "fn" -> Some Token.Fn
  | "return" -> Some Token.Return
  | _ -> None

let rec tokenize cs = List.rev (t cs [] (1, 1))

and t cs (ts : Token.t list) ((row, col) as head) =
  match cs with
  | [] -> ts
  | c :: cs' ->
      if c = '\n' then t cs' ts (row + 1, 1)
      else if Char.Ascii.is_white c then t cs' ts (row, col + 1)
      else if Char.Ascii.is_letter c then t_name cs [] ts (head, head)
      else if Char.Ascii.is_digit c then t_num cs 0 ts (head, head)
      else if c = '#' then t_comment cs ts head
      else if Char.Ascii.is_print c then t_op cs ts (head, head)
      else raise (Errors.Unexpected { v = "character"; span = (head, head) })

and t_name cs s0 ts ((start, ((row, col) as head)) as span) =
  match cs with
  | c :: cs' when Char.Ascii.is_alphanum c ->
      t_name cs' (c :: s0) ts (start, (row, col + 1))
  | _ ->
      let name = String.of_seq (List.to_seq (List.rev s0)) in
      let token = match n2t name with Some t -> t | None -> Token.Name name in
      t cs ({ v = token; span } :: ts) head

and t_num cs n0 ts ((start, ((row, col) as head)) as span) =
  match cs with
  | c :: cs' when Char.Ascii.is_digit c ->
      t_num cs'
        ((10 * n0) + Char.Ascii.digit_to_int c)
        ts
        (start, (row, col + 1))
  | _ -> t cs ({ v = Token.Num n0; span } :: ts) head

and t_comment cs acc (row, col) =
  match cs with
  | c :: cs' when c <> '\n' -> t_comment cs' acc (row, col + 1)
  | _ -> t cs acc (row, col)

and t_op cs ts ((start_loc, (row, col)) as span) =
  match cs with
  | [] -> assert false
  | c :: cs' -> (
      match c2t c with
      | Some token -> t cs' ({ v = token; span } :: ts) (row, col + 1)
      | None -> raise (Errors.Unexpected { v = "char"; span }))
