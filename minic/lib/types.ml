type t =
  | Int
  | Float
  | Bool
  | Char
  | Void
  | List of t
  | Fn of t * t
  | Tuple of t list

let rec t_to_str (t : t) : string =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | Void -> "void"
  | List t' -> t_to_str t' ^ "[]"
  | Fn (t1, t2) -> Printf.sprintf "%s -> %s" (t_to_str t1) (t_to_str t2)
  | Tuple ts ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map t_to_str ts))
