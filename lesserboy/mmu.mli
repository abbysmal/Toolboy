exception Unimplemented of string

val get : Machine.t -> int -> Uint8.t
val get_u16 : Machine.t -> int -> int
val put : Machine.t -> int -> Uint8.t -> unit
val put_u16 : Machine.t -> int -> int -> unit
