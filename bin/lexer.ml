let c2t c =
  match c with
  | '(' -> Token.Lparen
  | ')' -> Token.Rparen
  | '=' -> Token.Eq
  | '>' -> Token.Gt
  | '<' -> Token.Lt
  | '+' -> Token.Add
  | '-' -> Token.Sub
  | '*' -> Token.Mul
  | '/' -> Token.Div
  | '&' -> Token.And
  | '|' -> Token.Or
  | '!' -> Token.Not
  | '^' -> Token.Xor
  | _ -> raise (Failure "unrecognized operator")

let n2t n =
  match n with
  | "let" -> Some Token.Let
  | "print" -> Some Token.Print
  | "true" -> Some Token.True
  | "false" -> Some Token.False
  | "if" -> Some Token.If
  | "then" -> Some Token.Then
  | "end" -> Some Token.End
  | _ -> None

let rec tokenize cs = List.rev (t cs [])

and t cs acc =
  match cs with
  | [] -> acc
  | c :: cs' ->
      if Char.Ascii.is_white c then t cs' acc
      else if Char.Ascii.is_letter c then t_name cs "" acc
      else if Char.Ascii.is_digit c then t_num cs 0 acc
      else if Char.Ascii.is_print c then t_op cs acc
      else raise (Failure "illegal character")

and t_name cs vs acc =
  match cs with
  | c :: cs' when Char.Ascii.is_alphanum c ->
      t_name cs' (vs ^ Char.escaped c) acc
  | _ ->
      let token = match n2t vs with Some t -> t | None -> Token.Name vs in
      t cs (token :: acc)

and t_num cs n0 acc =
  match cs with
  | [] -> t cs (Token.Num n0 :: acc)
  | c :: cs' ->
      if Char.Ascii.is_digit c then
        t_num cs' ((10 * n0) + Char.Ascii.digit_to_int c) acc
      else t cs (Token.Num n0 :: acc)

and t_one token cs acc =
  match cs with
  | [] -> raise (Failure "Unreachable")
  | c :: cs' -> t cs' (token :: acc)

and t_op cs acc =
  match cs with
  | [] -> raise (Failure "Unreachable")
  | c :: cs' -> t cs' (c2t c :: acc)

let print_tokens tokens =
  let print_token token =
    match token with
    | Token.Num n -> Printf.printf "Num %d\n" n
    | Token.Name s -> Printf.printf "Name %s\n" s
    | Token.True -> Printf.printf "True\n"
    | Token.False -> Printf.printf "False\n"
    | Token.Lparen -> Printf.printf "Lparen\n"
    | Token.Rparen -> Printf.printf "Rparen\n"
    | Token.Eq -> Printf.printf "Eq\n"
    | Token.Gt -> Printf.printf "Gt\n"
    | Token.Lt -> Printf.printf "Lt\n"
    | Token.Add -> Printf.printf "Add\n"
    | Token.Sub -> Printf.printf "Sub\n"
    | Token.Mul -> Printf.printf "Mul\n"
    | Token.Div -> Printf.printf "Div\n"
    | Token.And -> Printf.printf "And\n"
    | Token.Or -> Printf.printf "Or\n"
    | Token.Not -> Printf.printf "Not\n"
    | Token.Xor -> Printf.printf "Xor\n"
    | Token.Let -> Printf.printf "Let\n"
    | Token.Print -> Printf.printf "Print\n"
    | Token.If -> Printf.printf "If\n"
    | Token.Then -> Printf.printf "Then\n"
    | Token.End -> Printf.printf "End\n"
  in
  List.iter print_token tokens
