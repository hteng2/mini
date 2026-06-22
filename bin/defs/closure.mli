type 'a t

val empty : unit -> 'a t
val copy : 'a t -> 'a t

val get : 'a t -> string -> 'a option
val set : 'a t -> string -> 'a -> unit
val del : 'a t -> string -> unit

val merge : 'a t -> 'a t -> unit
val iter : (string -> 'a -> unit) -> 'a t -> unit

val search : 'a t list -> string -> 'a option
