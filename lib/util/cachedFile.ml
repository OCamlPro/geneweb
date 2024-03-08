open Bigarray

type t = {
  fd : Unix.file_descr;
  mutable offset : int;
  filea : (char, Bigarray.int8_unsigned_elt, c_layout) Array1.t;
}

module StrMap = Map.Make (String)

let cached = ref StrMap.empty

let open_ro fname : t =
  let f = Filename.basename fname in
  match StrMap.find_opt f !cached with
  | Some t ->
      t.offset <- 0;
      t
  | None ->
      let fd = Secure.open_in_bin fname |> Unix.descr_of_in_channel in
      let filea =
        Unix.map_file fd char c_layout false [| -1 |] |> array1_of_genarray
      in
      let t = { offset = 0; filea; fd } in
      cached := StrMap.add f t !cached;
      t

let seek t offset : unit = t.offset <- offset
let pos t = t.offset
let close _t : unit = () (* Unix.close t.fd *)

let check_end t size =
  let asize = Array1.dim t.filea in
  if asize < t.offset + size then (
    t.offset <- asize;
    raise End_of_file)
  else ()

let get_byte t =
  let off = t.offset in
  t.offset <- off + 1;
  Array1.get t.filea off |> Char.code

let get_bytes t size =
  check_end t size;
  let off = t.offset in
  t.offset <- t.offset + size;
  Bytes.create size |> Bytes.mapi (fun i _ -> Array1.get t.filea (off + i))

let store_bytes t buf pos len =
  let b' = get_bytes t len in
  Bytes.blit b' 0 buf pos len

let read t size : string = get_bytes t size |> String.of_bytes

let read_binary_int t : int =
  check_end t 4;
  let b = get_bytes t 4 in
  Bytes.get_int32_be b 0 |> Int32.to_int

let read_value t : 'a =
  let h = get_bytes t Marshal.header_size in
  let size = Marshal.data_size h 0 in
  let buf = Bytes.create (Marshal.total_size h 0) in
  Bytes.blit h 0 buf 0 Marshal.header_size;
  let data = get_bytes t size in
  Bytes.blit data 0 buf Marshal.header_size size;
  Marshal.from_bytes buf 0

(*****)

(* let sizeof_long = 4 *)
let sign_extend_shift = (((Sys.word_size / 8) - 1) * 8) - 1
let sign_extend x = (x lsl sign_extend_shift) asr sign_extend_shift
let prefix_SMALL_BLOCK = 0x80
let prefix_SMALL_INT = 0x40
let prefix_SMALL_STRING = 0x20
let code_INT8 = 0x0
let code_INT16 = 0x1
let code_INT32 = 0x2
let code_INT64 = 0x3
let code_BLOCK32 = 0x8
let code_BLOCK64 = 0x13
let code_STRING8 = 0x9
let code_STRING32 = 0xA

type 'a in_funs = {
  input_byte : 'a -> int;
  input_binary_int : 'a -> int;
  input : 'a -> bytes -> int -> int -> unit;
}

let input_binary_int64 ifuns ic =
  let rec loop cnt n =
    if cnt = 0 then n else loop (cnt - 1) ((n lsl 8) + ifuns.input_byte ic)
  in
  loop 8 0

let rec input_loop ifuns ic =
  let code = ifuns.input_byte ic in
  if code >= prefix_SMALL_INT then
    if code >= prefix_SMALL_BLOCK then
      input_block ifuns ic (code land 0xf) ((code lsr 4) land 0x7)
    else Obj.magic (code land 0x3f)
  else if code >= prefix_SMALL_STRING then (
    let len = code land 0x1F in
    let s = Bytes.create len in
    ifuns.input ic s 0 len;
    Obj.magic s)
  else if code = code_INT8 then Obj.magic (sign_extend (ifuns.input_byte ic))
  else if code = code_INT16 then
    let h = ifuns.input_byte ic in
    Obj.magic ((sign_extend h lsl 8) + ifuns.input_byte ic)
  else if code = code_INT32 then
    let x1 = ifuns.input_byte ic in
    let x2 = ifuns.input_byte ic in
    let x3 = ifuns.input_byte ic in
    let x4 = ifuns.input_byte ic in
    Obj.magic ((sign_extend x1 lsl 24) + (x2 lsl 16) + (x3 lsl 8) + x4)
  else if code = code_INT64 then
    let () = assert (Sys.word_size = 64) in
    Obj.magic (input_binary_int64 ifuns ic)
  else if code = code_BLOCK32 then
    let header = ifuns.input_binary_int ic in
    Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
  else if code = code_BLOCK64 then
    if Sys.word_size = 64 then
      let header = input_binary_int64 ifuns ic in
      Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
    else failwith "input bad code block 64"
  else if code = code_STRING8 then (
    let len = ifuns.input_byte ic in
    let s = Bytes.create len in
    ifuns.input ic s 0 len;
    Obj.magic s)
  else if code = code_STRING32 then (
    let len = ifuns.input_binary_int ic in
    let s = Bytes.create len in
    ifuns.input ic s 0 len;
    Obj.magic s)
  else failwith (Printf.sprintf "input bad code 0x%x" code)

and input_block ifuns ic tag size =
  let v =
    if tag = 0 then Obj.magic (Array.make size (Obj.magic 0))
    else Obj.new_block tag size
  in
  for i = 0 to size - 1 do
    let x = input_loop ifuns ic in
    Obj.set_field v i (Obj.magic x)
  done;
  v

let in_channel_funs =
  {
    input_byte = get_byte;
    input_binary_int = read_binary_int;
    input = store_bytes;
  }

let read_value_gw t : 'a = Obj.magic @@ input_loop in_channel_funs t
