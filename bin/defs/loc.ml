type loc = string * int * int
type range = string * (int * int) * (int * int)
type 'a located = { v : 'a; loc : loc }
type 'a spanned = { v : 'a; span : range }
