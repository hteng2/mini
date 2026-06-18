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

let rec tokenize (src : char Stream.t) : Token.t Stream.t = t src (1, 1)

and t (src : char Stream.t) ((row, col) as head : int * int) : Token.t Stream.t
    =
  Stream.push (fun () ->
      match Stream.pop src with
      | Stream.End -> Stream.End
      | Stream.Head (c, src') as front ->
          let src = Stream.push (fun () -> front) in
          if c = '\n' then Stream.pop (t src' (row + 1, 1))
          else if Char.Ascii.is_white c then Stream.pop (t src' (row, col + 1))
          else if Char.Ascii.is_letter c then t_name src [] (head, head)
          else if Char.Ascii.is_digit c then t_num src 0 (head, head)
          else if c = '#' then t_comment src head
          else if Char.Ascii.is_print c then t_op src (head, head)
          else
            raise (Errors.Unexpected { v = "character"; span = (head, head) }))

and t_name (src : char Stream.t) (s0 : char list)
    ((start, ((row, col) as head)) as span : (int * int) * (int * int)) :
    Token.t Stream.front =
  match Stream.pop src with
  | Stream.Head (c, src') when Char.Ascii.is_alphanum c ->
      t_name src' (c :: s0) (start, (row, col + 1))
  | front ->
      let name = String.of_seq (List.to_seq (List.rev s0)) in
      let token = match n2t name with Some t -> t | None -> Token.Name name in
      let token' = ({ v = token; span } : Token.t) in
      let old_src = Stream.push (fun () -> front) in
      Stream.Head (token', t old_src head)

and t_num src n0 ((start, ((row, col) as head)) as span) =
  match Stream.pop src with
  | Stream.Head (c, src') when Char.Ascii.is_digit c ->
      t_num src' ((10 * n0) + Char.Ascii.digit_to_int c) (start, (row, col + 1))
  | front ->
      Stream.Head
        ( ({ v = Token.Num n0; span } : Token.t),
          t (Stream.push (fun () -> front)) head )

and t_comment src (row, col) =
  match Stream.pop src with
  | Stream.Head (c, src') when c <> '\n' -> t_comment src' (row, col + 1)
  | front -> Stream.pop (t (Stream.push (fun () -> front)) (row, col))

and t_op src ((start_loc, (row, col)) as span) =
  match Stream.pop src with
  | Stream.End -> assert false
  | Stream.Head (c, src') -> (
      match c2t c with
      | None -> raise (Errors.Unexpected { v = "char"; span })
      | Some token ->
          Stream.Head
            ( ({ v = token; span = (start_loc, (row, col + 1)) } : Token.t),
              t src' (row, col + 1) ))
