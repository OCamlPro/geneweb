(** Same as {!Stdlib.Map.S.key} *)

type ('key, +'a) t
(** Same as {!Stdlib.Map.S.t} *)

val mem : ('key -> 'key -> int) -> 'key -> ('key, 'a) t -> bool
(** Same as {!Stdlib.Map.S.mem} *)

val add : ('key -> 'key -> int) -> 'key -> 'a -> ('key, 'a) t -> ('key, 'a) t
(** Same as {!Stdlib.Map.S.add} *)

val find : ('key -> 'key -> int) -> 'key -> ('key, 'a) t -> 'a
(** Same as {!Stdlib.Map.S.find} *)

val key_after : ('key -> int) -> ('key, 'a) t -> 'key
(** [key_after f_compare m] browse map [m] to find the 'key [k] which
        gives [f_compare k = 0]. Raise [Not_found] if such 'key doesn't exists. *)

val next : ('key -> 'key -> int) -> 'key -> ('key, 'a) t -> 'key
(** [next k bt] returns the smallest 'key that is bigger then [k] inside [bt]. *)
