type 'a t = unit -> 'a front
and 'a front = End | Head of 'a * 'a t
