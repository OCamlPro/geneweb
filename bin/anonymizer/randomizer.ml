(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 Steven de Oliveira <de.oliveira.steven@gmail.com>  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Def

module Random = struct
  let int i = try Random.int i with _ -> failwith (Format.sprintf "Bad range %i" i)
  let bool = Random.bool
end

let lorem =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed a felis eget \
   odio pulvinar consequat viverra eget lorem. Phasellus imperdiet justo eget \
   neque pellentesque, et dictum metus finibus. Etiam egestas ac tellus a \
   tristique. Nulla facilisi. Phasellus volutpat lectus eu justo placerat \
   cursus. Fusce congue placerat lorem ac facilisis. Etiam vehicula euismod \
   arcu. Donec fermentum justo vel enim condimentum tempus. Pellentesque quis \
   scelerisque urna, euismod interdum purus. Proin eget auctor turpis, non \
   lacinia ex. Nam ante dui, rutrum vestibulum pretium in, pellentesque non \
   dui.Fusce pharetra vitae lectus vel congue. Vivamus at nunc non neque \
   tempus pulvinar. Nullam vitae nunc gravida, congue urna id, aliquam velit. \
   Aliquam sed viverra nibh. Suspendisse potenti. Sed ultrices diam nisi, eu \
   pharetra nulla vestibulum id. Aenean fringilla, dui eu consectetur \
   efficitur, lectus orci fermentum lacus, vitae efficitur ipsum neque quis \
   ligula. Etiam dignissim accumsan neque, at ornare sem posuere eget. Vivamus \
   et mi venenatis, molestie augue non, auctor orci. Fusce euismod, ligula vel \
   tristique gravida, tellus turpis viverra libero, eget lacinia orci neque \
   vitae nisi. Nullam sapien nulla, hendrerit et metus vitae, tempor pulvinar \
   sapien. Nulla vel varius odio, sit amet volutpat ex. Curabitur eu lorem eu \
   diam pulvinar ultricies maximus vel nunc. Morbi eget eleifend libero, sit \
   amet aliquam lacus. Sed vel varius est. Morbi at elementum tellus, vitae \
   facilisis tellus. Mauris vitae lacus sit amet lorem ultricies mattis. \
   Pellentesque consequat consectetur quam eget lacinia. Suspendisse lacinia \
   nec leo sed ultrices. Donec nec urna finibus, pretium neque sit amet, \
   ultricies dolor. Ut imperdiet nibh massa. Nullam nec eros quis nulla \
   elementum mattis. In blandit turpis eget dui faucibus tristique. Quisque a \
   ante id libero consequat posuere. Vivamus lacinia nisl a felis fringilla \
   blandit. Nunc semper mi quis eleifend pharetra. Proin blandit in ante ut \
   rutrum. Sed erat ex, iaculis et aliquam in, sodales at tortor. Ut bibendum \
   non libero quis commodo. Sed sagittis, sapien vitae facilisis maximus, \
   dolor sem tempor neque, sed dignissim tortor risus vel arcu. Mauris sit \
   amet nisl in mi molestie dictum eget sed risus. Fusce at elit molestie, \
   mattis est eu, mollis sem. Donec non magna id tellus rutrum tincidunt. \
   Nullam vehicula euismod lectus, at cursus dui scelerisque vitae. Interdum \
   et malesuada fames ac ante ipsum primis in faucibus. Donec fringilla, \
   mauris vel pulvinar venenatis, leo orci hendrerit tortor, ac blandit augue \
   libero eu nisi. Suspendisse non lobortis purus, nec imperdiet massa. Nunc \
   vel eleifend sem. Donec posuere eget nisi eu fringilla. Sed gravida \
   vulputate cursus. Vestibulum congue venenatis lorem ac luctus. Nullam id \
   neque faucibus nisl sodales euismod vitae ut sem. Vivamus ligula sapien, \
   convallis sed lectus eget, tincidunt pulvinar nunc. Praesent facilisis \
   ullamcorper fringilla. Donec condimentum molestie facilisis. Sed nec eros \
   fringilla, sollicitudin ex a, volutpat odio. Sed ornare tincidunt nibh nec \
   rutrum. Suspendisse lobortis, nunc non consectetur vehicula, leo quam \
   vulputate metus, nec dictum elit lacus a tellus. Nulla lorem nunc, \
   vestibulum non libero eu, euismod porttitor orci. Proin ut egestas urna. Ut \
   a hendrerit est. Vestibulum eleifend dolor a ante euismod pretium. \
   Suspendisse ultrices eros iaculis nisi pretium, porta aliquet felis \
   efficitur."

let lorem_size = String.length lorem

let names =
  [| "Albert"; "Bernard"; "Cyrille"; "Daniel"; "Éric"; "François"; "Gérard";
     "Hervé"; "Isidore"; "Jacques"; "Kevin"; "Louis"; "Michel"; "Nicolas";
     "Octave"; "Philippe"; "Quentin"; "René"; "Sylvain"; "Thierry"; "Urbain";
     "Vincent"; "Wolfgang"; "Xavier"; "Yann"; "Zébulon";
     "Anne"; "Brigitte"; "Cécile"; "Denise"; "Emmanuelle"; "Fanny";
     "Geneviève"; "Hélène"; "Isabelle"; "Joëlle"; "Karine"; "Lise"; "Marie";
     "Noëlle"; "Odile"; "Patricia"; "Quitterie"; "Rosine"; "Sidonie";
     "Thérèse"; "Ursule"; "Vanessa"; "Wilfried"; "Xavière"; "Yvonne"; "Zoé" |]

let name_table : (string, string) Hashtbl.t = Hashtbl.create 101
let lastname_table : (string, string) Hashtbl.t = Hashtbl.create 101

(* Selects a random substring of the lorem string of size size *)
let loremize size =
  if size >= lorem_size
  then lorem
  else
    let offset = Random.int (lorem_size - size) in
    String.sub lorem offset size

(* Given a string of size 'r', returns a random
   substring of lorem of size 'r' *)
let loremize_str str =
  loremize (String.length str)

let pure_random_name () =
  let fn = names.(Random.int 52) in
  let ln = names.(Random.int 52) in
  Format.sprintf "%s %s" fn ln

let add_new_name table key =
  let name = names.(Random.int 52) in
  Hashtbl.add table key name;
  name

let get_new_name table name =
  if name = "" then "" else
  match Hashtbl.find_opt name_table name with
  | Some name -> name
  | None -> add_new_name name_table name

let get_new_fname = get_new_name name_table
let get_new_lname = get_new_name lastname_table

let random_name_from_name fname lname =
  let new_fname = get_new_fname fname in
  let new_lname = get_new_lname lname in
  new_fname, new_lname

(*  Format.sprintf "%s /%s/" new_fname new_lname *)

(* If the name has the format FNAME FNAME2 ... /LNAME/, transforms it into
   the same name format. Otherwise, returns a random FNAME LNAME. *)
let random_name old_name =
  match List.rev (String.split_on_char ' ' old_name) with
  | lname :: fname when Str.string_match (Str.regexp "/.*/") lname 0 -> begin
      (* Name format: FNAME FNAME2 ... /LNAME/ *)
      match String.split_on_char '/' lname with
      | [""; real_lname; ""] ->
        let fname, lname = random_name_from_name (String.concat " " (List.rev fname)) real_lname in
        Format.sprintf "%s /%s/" fname lname
      | _ -> assert false
    end
  | _ -> pure_random_name ()

(* Names with the format fname/lname/id return Some (fname, lname, Some id, "") (if is is an integer)
   Names with the format fname/lname/text return Some (fname, lname, None, text)
   Names with the format fname/lname return Some (fname, lname, None, None)
   Otherwise return None
*)
let get_name_from_reference_prefix str =
  match String.split_on_char '/' str with
  | fname :: lname :: id :: ((_ :: _) as txt) ->
    if Autil.is_int id
    then Some (fname, lname, Some id, (String.concat "/" txt))
    else Some (fname, lname, None, (String.concat "/" (id :: txt)))
  | [fname; lname; id] ->
    if Autil.is_int id then Some (fname, lname, Some id, "")
    else Some (fname, lname, None, id)
  | [fname; lname] -> Some (fname, lname, None, "")
  | _ -> None

(* Gets a reference content, returns the translated reference *)
let get_data_from_reference str =
  match String.split_on_char '|' str with
  | prefix :: ((_ :: _) as rest)  -> begin
      match get_name_from_reference_prefix prefix with
      | Some (fname, lname, id, "") ->
        let fname, lname = random_name_from_name fname lname in
        let text_size = List.fold_left (fun acc s -> acc + String.length s + 1) 0 rest in
        Format.asprintf "[[%a/%a%a|%s]]"
          Autil.pp_s fname
          Autil.pp_s lname
          (Autil.pp_opt (fun fmt e -> Format.fprintf fmt "/%s" e)) id
          (loremize (text_size - 1)) (* Minus one because we counted one | too much *)
      | _ ->
        Format.printf "Strange reference %s, discarding it" str;
        loremize_str str
    end

  | [_str] (*= str*) -> begin (* fname/lname/id/text *)
      match get_name_from_reference_prefix str with
      | Some (fname, lname, id, txt) ->
        let fname, lname = random_name_from_name fname lname in
        Format.asprintf "[[%a/%a%a/%s]]"
          Autil.pp_s fname
          Autil.pp_s lname
          (Autil.pp_opt (fun fmt e -> Format.fprintf fmt "/%s" e)) id
          (loremize_str txt)
      | _ ->
        Format.printf "Strange reference %s, discarding it" str;
        loremize_str str
    end

  | [] -> ""


(* Loremizes a wiki text (keeping compatibility with references) *)
let randomize_wiki_text str =
  (* Splitting the text enclosed by "[[" and "]]" ; expects to be well
     parenthesized. If str starts with "[[", we add "" at the beginning
     of the list *)
  let strs =
    let regexp = Str.regexp "\\[\\[\\|\\]\\]" in
    let substrings = Str.split regexp str in
    if
      String.length str <= 1
      || String.get str 0 <> '['
      || String.get str 1 <> '['
    then substrings
    else "" :: substrings
  in
  let anon_strs =
    List.mapi
      (fun i str ->
         if i land 1 = 0
         then (* text *) loremize_str str
         else get_data_from_reference str
      )
      strs
  in
  String.concat "" anon_strs

(* Todo: keep order of dates *)
let random_date (d : Adef.cdate) : Adef.cdate =
  try
    Adef.cdate_of_date @@
    match Adef.date_of_cdate d with
    | Dgreg (d, cal) ->
      let d' = Adef.{
        day =  Random.int 28;
        month = Random.int 12;
        year = Random.int (abs @@ 2 * d.year + 1);
        prec = About;
        delta = 0
      } in
      Dgreg (d', cal)
    | Dtext _ -> Dtext "unknown"
  with
    Failure s when s = "date_of_cdate" -> d

(* Returns a random location *)
let random_loc () = pure_random_name () ^ " " ^ pure_random_name () ^ " Land"

let random_death = function
  | Death (dr, d) -> Death (Unspecified, random_date d)
  | _ ->
    match Random.int 5 with
    | 0 -> NotDead
    | 1 -> DeadYoung
    | 2 -> DeadDontKnowWhen
    | 3 -> DontKnowIfDead
    | 4 -> OfCourseDead
    | _ -> assert false

let random_burial = function
  | UnknownBurial -> UnknownBurial
  | Buried d
  | Cremated d ->
    if Random.bool ()
    then Buried (random_date d)
    else Cremated (random_date d)

let random_divorce () =
  if Random.bool ()
  then NotDivorced
  else Separated
