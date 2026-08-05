type t = Int | Float | Bool | Char | Void | List of t | Fn of t * t list

let rec t_to_str (t : t) : string =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | Void -> "void"
  | List t' -> t_to_str t' ^ "[]"
  | Fn (t, ts) ->
      let args = String.concat ", " (List.map t_to_str ts) in
      Printf.sprintf "(%s) -> %s" args (t_to_str t)
