type loc = int * int
type range = string * (int * int) * (int * int)
type 'a spanned = { v : 'a; span : range }
