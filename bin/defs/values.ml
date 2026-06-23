type v =
  | Int of int
  | Bool of bool
  | Str of string
  | Void
  | List of v ref array
  | Fn of string list * value Closure.t * Ir.dec

and value = Var of v ref | Const of v

let value_to_v value = match value with Var v -> !v | Const v -> v
