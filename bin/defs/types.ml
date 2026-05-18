type tt = Int | Bool | List of tt
type tm = Const | Var
type t = tt * tm
type 'a typed = t * 'a
