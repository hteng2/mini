type value =
  | Int of int
  | Bool of bool
  | Void
  | List of value array
  | Fn of Ast.param list * Ast.dec
