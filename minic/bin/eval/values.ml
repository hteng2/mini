open Minic_lib

type value =
  | Nil
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Void
  | List of value array
  | Tuple of value list
  (* closure captures, sym count, body loc *)
  | Fn of value array * int * int
  | Builtin of (value -> value)
