(* Copyright (c) 1998-2007 INRIA *)

(* This module controls which directories can be read by the webserver, to
   prevent malicious users from reading/writing on the server disk. *)

(* Allowed directories are composed of [assets] and the [base_dir]. *)
val assets : unit -> string list
val base_dir : unit -> string

val add_assets : string -> unit
val set_base_dir : string -> unit

(** Check if a filename is safe to read:
    * it must not contain the '\000' character
    * it must either be relative to the local directory OR
      included in one of the allowed directories (base_dir or assets)
    * the relative part does not contain the '..' directory
*)
val check : string -> bool

(* The following functions perform a [check] before opening the file,
   preventing potential attacks on the system.
*)
val open_in : string -> in_channel
val open_in_bin : string -> in_channel
val open_out : string -> out_channel
val open_out_bin : string -> out_channel
val open_out_gen : open_flag list -> int -> string -> out_channel
