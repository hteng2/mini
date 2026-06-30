open Mini

type v =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Str of string
  | Void
  | List of v array
  | Fn of string list * value Closure.t * Ir2.dec
  | Builtin of (v list -> v)

and value = Var of v ref | Const of v

let value_to_v value = match value with Var v -> !v | Const v -> v
