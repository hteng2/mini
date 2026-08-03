open Mini

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Str of string
  | Void
  | List of value array
  | Fn of string list * value Closure.t * int
  | Builtin of (value list -> value)
