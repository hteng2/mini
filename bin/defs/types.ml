type tt = Untyped | Int | Bool | Void | List of tt | Fn of tt * tt list
type tm = Const | Var
type t = tt * tm
type 'a typed = t * 'a
