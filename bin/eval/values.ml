open Mini

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Str of string
  | Void
  | List of value array
  | Fn of int array * (int, value) Hashtbl.t * int * int
  | Builtin of (value array -> value)
