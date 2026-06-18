type 'a t
type 'a front = End | Head of 'a * 'a t

val empty : 'a t
val push : (unit -> 'a front) -> 'a t
val pop : 'a t -> 'a front
