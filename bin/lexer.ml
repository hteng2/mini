type token = Name of string | Num of int | Op of char

let rec tokenize cs = List.rev (t cs [])

and t cs acc =
  match cs with
  | [] -> acc
  | c :: cs' ->
      if Char.Ascii.is_white c then t cs' acc
      else if Char.Ascii.is_letter c then t_var cs "" acc
      else if Char.Ascii.is_digit c then t_num cs 0 acc
      else if Char.Ascii.is_print c then t_op cs acc
      else raise (Failure "unimplemented")

and t_var cs vs acc =
  match cs with
  | [] -> Name vs :: acc
  | c :: cs' ->
      if Char.Ascii.is_alphanum c then t_var cs' (vs ^ Char.escaped c) acc
      else t cs (Name vs :: acc)

and t_num cs n0 acc =
  match cs with
  | [] -> Num n0 :: acc
  | c :: cs' ->
      if Char.Ascii.is_digit c then
        t_num cs' ((10 * n0) + Char.Ascii.digit_to_int c) acc
      else t cs (Num n0 :: acc)

and t_op cs acc =
  match cs with
  | [] -> raise (Failure "unreachable")
  | c :: cs' -> t cs' (Op c :: acc)

let print_tokens tokens =
  let print_token token =
    match token with
    | Name s -> Printf.printf "Var %s\n" s
    | Num n -> Printf.printf "Num %d\n" n
    | Op c -> Printf.printf "Op %c\n" c
  in
  List.iter print_token tokens
