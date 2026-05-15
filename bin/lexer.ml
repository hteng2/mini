let c2t c =
  match c with
  | '(' -> Some Token.Lparen
  | ')' -> Some Token.Rparen
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
  | "print" -> Some Token.Print
  | "true" -> Some Token.True
  | "false" -> Some Token.False
  | "if" -> Some Token.If
  | "then" -> Some Token.Then
  | "else" -> Some Token.Else
  | "end" -> Some Token.End
  | "while" -> Some Token.While
  | "do" -> Some Token.Do
  | "done" -> Some Token.Done
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

let print_tokens ts =
  let t2s t =
    match t with
    | Token.Num n -> Printf.sprintf "Num %d\t" n
    | Token.Name n -> Printf.sprintf "Name %s\t" n
    | Token.True -> "True\t"
    | Token.False -> "False\t"
    | Token.Lparen -> "Lparen\t"
    | Token.Rparen -> "Rparen\t"
    | Token.Comma -> "Comma\t"
    | Token.Eq -> "Eq\t"
    | Token.Gt -> "Gt\t"
    | Token.Lt -> "Lt\t"
    | Token.Add -> "Add\t"
    | Token.Sub -> "Sub\t"
    | Token.Mul -> "Mul\t"
    | Token.Div -> "Div\t"
    | Token.Mod -> "Mod\t"
    | Token.And -> "And\t"
    | Token.Or -> "Or\t"
    | Token.Not -> "Not\t"
    | Token.Xor -> "Xor\t"
    | Token.Let -> "Let\t"
    | Token.Var -> "Var\t"
    | Token.Print -> "Print\t"
    | Token.If -> "If\t"
    | Token.Then -> "Then\t"
    | Token.Else -> "Else\t"
    | Token.End -> "End\t"
    | Token.While -> "While\t"
    | Token.Do -> "Do\t"
    | Token.Done -> "Done\t"
  in
  List.fold_left
    (fun _ ->
      fun ({ v; span = (sr, sc), (er, ec) } : Token.t) ->
       Printf.printf "%s %d:%d-%d:%d\n" (t2s v) sr sc er ec;
       ())
    () ts
