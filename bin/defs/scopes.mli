exception NameError of string
type 'a t
val add_scope : 'a t list -> 'a t list
val search_top : 'a t list -> string -> 'a option
val search_scopes : 'a t list -> string -> 'a
val update_scopes : 'a t list -> string -> 'a -> unit
val add_to_scope : 'a t list -> string -> 'a -> unit
val copy_scopes : 'a t list -> 'a t list
