type 'a t = unit -> 'a front
and 'a front = End | Head of 'a * 'a t

let empty = fun () -> End
let push f = f
let pop f = f ()
