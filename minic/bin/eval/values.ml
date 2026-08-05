open Minic_lib

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Void
  | List of value array
  | Fn of value array * int
  | Builtin of (value array -> value)
