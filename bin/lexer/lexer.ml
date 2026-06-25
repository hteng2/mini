open Mini

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
  | _ -> None

let n2t n =
  match n with
  | "let" -> Some Token.Let
  | "var" -> Some Token.Var
  | "true" -> Some Token.True
  | "false" -> Some Token.False
  | "if" -> Some Token.If
  | "else" -> Some Token.Else
  | "while" -> Some Token.While
  | "break" -> Some Token.Break
  | "continue" -> Some Token.Continue
  | "fn" -> Some Token.Fn
  | "return" -> Some Token.Return
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
      else if Char.Ascii.is_digit c then t_num src sn 0 (head, head)
      else if c = '#' then t_comment src sn head
      else if Char.Ascii.is_print c then t_op src sn (head, head)
      else
        raise
          (Errors.Unexpected
             { v = "character"; span = (sn, head, move_head c head) })

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

and t_num src sn n0 (start, head) =
  match src () with
  | Stream.Head (c, src') when Char.Ascii.is_digit c ->
      t_num src' sn
        ((10 * n0) + Char.Ascii.digit_to_int c)
        (start, move_head c head)
  | front ->
      Stream.Head
        ( { v = Token.Num n0; span = (sn, start, head) },
          t (fun () -> front) sn head )

and t_comment src sn (row, col) =
  match src () with
  | Stream.Head (c, src') when c <> '\n' -> t_comment src' sn (row, col + 1)
  | front -> (t (fun () -> front) sn (row, col)) ()

and t_op src sn (start, head) =
  match src () with
  | Stream.End -> assert false
  | Stream.Head (c, src') -> (
      match c2t c with
      | None ->
          raise (Errors.Unexpected { v = "char"; span = (sn, start, head) })
      | Some token ->
          Stream.Head
            ( { v = token; span = (sn, start, move_head c head) },
              t src' sn (move_head c head) ))
