type tt =
  | Untyped
  | Int
  | Bool
  | Char
  | Str
  | Void
  | List of tt
  | Fn of tt * tt list

type tm = Const | Var
type t = tt * tm
