open Mini

let n2t n =
  match n with
  | "let" -> Some Token.Bind
  | "true" -> Some Token.True
  | "false" -> Some Token.False
  | "if" -> Some Token.If
  | "else" -> Some Token.Else
  | "fn" -> Some Token.Fn
  | "import" -> Some Token.Import
  | _ -> None

let to_escaped c =
  match c with 'n' -> Some '\n' | 't' -> Some '\t' | _ -> None

let move_head c (row, col) =
  match c with '\n' -> (row + 1, 1) | _ -> (row, col + 1)

let rec tokenize src sn = t src sn (1, 1)

and t src sn head =
 fun () ->
  match src () with
  | Stream.End -> Stream.End
  | Stream.Head (c, src') as front ->
      let src = fun () -> front in
      if Char.Ascii.is_white c then (t src' sn (move_head c head)) ()
      else if Char.Ascii.is_letter c || c = '_' then
        t_name src sn [] (head, head)
      else if c = '"' then t_string src' sn [] (head, move_head c head)
      else if c = '\'' then t_char src' sn (head, move_head c head)
      else if Char.Ascii.is_digit c then t_int src sn 0 (head, head)
      else if c = '#' then t_comment src sn head
      else t_op src sn (head, head)

and t_name src sn s0 (start, head) =
  match src () with
  | Stream.Head (c, src') when Char.Ascii.is_alphanum c || c = '_' ->
      t_name src' sn (c :: s0) (start, move_head c head)
  | front ->
      let name = String.of_seq (List.to_seq (List.rev s0)) in
      let token = match n2t name with Some t -> t | None -> Token.Name name in
      let token' = ({ v = token; span = (sn, start, head) } : Token.t) in
      let old_src = fun () -> front in
      Stream.Head (token', t old_src sn head)

and t_string src sn s0 (start, head) =
  match src () with
  | Stream.Head ('"', src') ->
      let str = String.of_seq (List.to_seq (List.rev s0)) in
      Stream.Head
        ( { v = Token.Str str; span = (sn, start, move_head '"' head) },
          t src' sn (move_head '"' head) )
  | Stream.Head ('\\', src') -> (
      let head = move_head '\\' head in
      match src' () with
      | Stream.Head (c2, src'') -> (
          match to_escaped c2 with
          | Some c2' -> t_string src'' sn (c2' :: s0) (start, move_head c2 head)
          | None ->
              raise
                (Errors.Unexpected
                   {
                     v = "escape character";
                     span = (sn, head, move_head c2 head);
                   }))
      | _ ->
          raise
            (Errors.Expected
               { v = "closing \""; span = (sn, start, move_head '"' start) }))
  | Stream.Head (c, src') -> t_string src' sn (c :: s0) (start, move_head c head)
  | Stream.End ->
      raise
        (Errors.Expected
           { v = "closing \""; span = (sn, start, move_head '"' start) })

and t_char src sn (start, head) =
  match src () with
  | Stream.Head ('\'', src') ->
      raise (Errors.Expected { v = "character"; span = (sn, start, head) })
  | Stream.End ->
      raise (Errors.Expected { v = "character"; span = (sn, start, head) })
  | Stream.Head ('\\', src') -> (
      let head = move_head '\\' head in
      match src' () with
      | Stream.Head (c2, src'') -> (
          match to_escaped c2 with
          | None ->
              raise
                (Errors.Unexpected
                   {
                     v = "escape character";
                     span = (sn, head, move_head c2 head);
                   })
          | Some c2' -> (
              let head = move_head c2 head in
              match src'' () with
              | Stream.Head ('\'', src''') ->
                  Stream.Head
                    ( {
                        v = Token.Char c2';
                        span = (sn, start, move_head '\'' head);
                      },
                      t src''' sn (move_head '\'' head) )
              | _ ->
                  raise
                    (Errors.Expected
                       { v = "matching \'"; span = (sn, start, head) })))
      | Stream.End ->
          raise
            (Errors.Expected
               { v = "matching \'"; span = (sn, start, move_head '"' start) }))
  | Stream.Head (c, src') -> (
      match src' () with
      | Stream.Head ('\'', src''') ->
          Stream.Head
            ( { v = Token.Char c; span = (sn, start, move_head '\'' head) },
              t src''' sn (move_head '\'' head) )
      | _ ->
          raise
            (Errors.Expected
               { v = "matching \'"; span = (sn, start, move_head '"' start) }))

and t_int src sn n0 (start, head) =
  match src () with
  | Stream.Head (c, src') when Char.Ascii.is_digit c ->
      t_int src' sn
        ((10 * n0) + Char.Ascii.digit_to_int c)
        (start, move_head c head)
  | Stream.Head (c, src') when c = '.' ->
      t_float src' sn (Float.of_int n0) 0.1 (start, move_head c head)
  | front ->
      Stream.Head
        ( { v = Token.Int n0; span = (sn, start, head) },
          t (fun () -> front) sn head )

and t_float src sn n0 d (start, head) =
  match src () with
  | Stream.Head (c, src') when Char.Ascii.is_digit c ->
      t_float src' sn
        (Float.add n0 (Float.mul d (Float.of_int (Char.Ascii.digit_to_int c))))
        (Float.div d 10.0)
        (start, move_head c head)
  | front ->
      Stream.Head
        ( { v = Token.Float n0; span = (sn, start, head) },
          t (fun () -> front) sn head )

and t_comment src sn (row, col) =
  match src () with
  | Stream.Head (c, src') when c <> '\n' -> t_comment src' sn (row, col + 1)
  | front -> (t (fun () -> front) sn (row, col)) ()

and t_op src sn (start, head) =
  let lex_2 src (sn, start, head) t0 c1 t1 =
    match src () with
    | Stream.Head (c, src') when c = c1 ->
        Stream.Head
          ( ({ v = t1; span = (sn, start, move_head c1 head) } : Token.t),
            t src' sn (move_head c1 head) )
    | front ->
        Stream.Head
          ({ v = t0; span = (sn, start, head) }, t (fun () -> front) sn head)
  in
  match src () with
  | Stream.End -> assert false
  | Stream.Head (c, src') -> (
      let head = move_head c head in
      let span = (sn, start, head) in
      match c with
      | '(' -> Stream.Head ({ v = Token.Lparen; span }, t src' sn head)
      | ')' -> Stream.Head ({ v = Token.Rparen; span }, t src' sn head)
      | '[' -> Stream.Head ({ v = Token.Lbrack; span }, t src' sn head)
      | ']' -> Stream.Head ({ v = Token.Rbrack; span }, t src' sn head)
      | '{' -> Stream.Head ({ v = Token.Lbrace; span }, t src' sn head)
      | '}' -> Stream.Head ({ v = Token.Rbrace; span }, t src' sn head)
      | ',' -> Stream.Head ({ v = Token.Comma; span }, t src' sn head)
      | ';' -> Stream.Head ({ v = Token.Semicolon; span }, t src' sn head)
      | '=' -> Stream.Head ({ v = Token.Eq; span }, t src' sn head)
      | '>' -> lex_2 src' span Token.Gt '=' Token.Ge
      | '<' -> lex_2 src' span Token.Lt '=' Token.Le
      | '+' -> Stream.Head ({ v = Token.Add; span }, t src' sn head)
      | '-' -> Stream.Head ({ v = Token.Sub; span }, t src' sn head)
      | '*' -> Stream.Head ({ v = Token.Mul; span }, t src' sn head)
      | '/' -> Stream.Head ({ v = Token.Div; span }, t src' sn head)
      | '%' -> Stream.Head ({ v = Token.Mod; span }, t src' sn head)
      | '&' -> Stream.Head ({ v = Token.And; span }, t src' sn head)
      | '|' -> Stream.Head ({ v = Token.Or; span }, t src' sn head)
      | '^' -> Stream.Head ({ v = Token.Xor; span }, t src' sn head)
      | '!' -> lex_2 src' span Token.Not '=' Token.Neq
      | _ -> raise (Errors.Unexpected { v = "char"; span = (sn, start, head) }))
