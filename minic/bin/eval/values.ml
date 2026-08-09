open Minic_lib

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Void
  | List of value array
  | Tuple of value list
  | Fn of value array * int
  | Builtin of (value -> value)
