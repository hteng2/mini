type t = Int | Bool | Tuple of t list
type 'a typed = t * 'a
