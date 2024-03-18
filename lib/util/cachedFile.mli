type t

val open_ro : string -> t
val close : t -> unit
val seek : t -> int -> unit
val pos : t -> int
val read : t -> int -> string
val read_binary_int : t -> int
val read_value : t -> 'a
val read_value_gw : t -> 'a
