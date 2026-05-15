type loc = int * int
type range = loc * loc
type 'a located = { v : 'a; loc : loc }
type 'a spanned = { v : 'a; span : range }
