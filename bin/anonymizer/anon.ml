module D = Gwdb_driver

let base_name : string ref = ref ""
let (base : Gwdb_driver.base ref) = ref @@ Obj.magic 0


let check_base_exists bname =
  if Array.mem bname (Sys.readdir ".")
  then begin
    let b = Gwdb_driver.open_base bname in
    base_name := bname;
    base := b;
  end else begin
    Format.printf "Base %s does not exist" bname;
    exit 2;
  end

let anonymize_fname (istr : D.istr) =
  let name = D.sou !base istr in
  let nname = Randomizer.get_new_fname name in
  D.insert_string !base nname

let anonymize_lname (istr : D.istr) =
  let name = D.sou !base istr in
  let nname = Randomizer.get_new_lname name in
  D.insert_string !base nname

let anonymize_death b d = assert false

let anonymize_text istr =
  let txt = D.sou !base istr in
  let txt' = Randomizer.randomize_wiki_text txt in
  D.insert_string !base txt'

let anonymize_loc istr =
  let txt' = Randomizer.random_loc () in
  D.insert_string !base txt'

let anonymize_gen_pers_event_name = function
  | Def.Epers_Name s -> Def.Epers_Name (anonymize_text s)
  | e -> e

let anonymize_gen_fam_event_name = function
  | Def.Efam_Name s -> Def.Efam_Name (anonymize_text s)
  | e -> e

let anonymize_pevent (e : (D.iper, D.istr) Def.gen_pers_event) =
  {e with
   epers_name = anonymize_gen_pers_event_name e.epers_name;
   epers_date = Randomizer.random_date e.epers_date;
   epers_place = anonymize_loc e.epers_place;
   epers_reason = anonymize_text e.epers_reason;
   epers_note = anonymize_text e.epers_note;
   epers_src = anonymize_text e.epers_src;
   (* epers_witnesses *)
  }

let anonymize_fevent (e : (D.iper, D.istr) Def.gen_fam_event) = {
  e with
  efam_name = anonymize_gen_fam_event_name e.efam_name;
  efam_date = Randomizer.random_date e.efam_date;
  efam_place = anonymize_loc ();
  efam_reason = anonymize_text e.efam_reason;
  efam_note = anonymize_text e.efam_note;
  efam_src = anonymize_text e.efam_src;
  (* efam_witnesses *)
}

let anonymize_person (person : D.person) =
  let p = D.gen_person_of_person person in
  let new_person =
    {p with
     first_name = anonymize_fname p.first_name;
     surname = anonymize_lname p.surname;
     (* occ no change *)
     image = D.empty_string;
     public_name = anonymize_lname p.public_name;
     qualifiers = [];
     aliases = List.map anonymize_fname p.aliases;
     first_names_aliases = List.map anonymize_fname p.first_names_aliases;
     surnames_aliases = List.map anonymize_lname p.surnames_aliases;
     (* rparents no change *)
     (* related no change *)
     (* sex *)
     (* access *)
     birth = Randomizer.random_date p.birth;
     birth_place = anonymize_loc ();
     birth_note = anonymize_text p.birth_note;
     birth_src = anonymize_text p.birth_src;
     baptism = Randomizer.random_date p.baptism;
     baptism_place = anonymize_loc ();
     baptism_note = anonymize_text p.baptism_note;
     baptism_src = anonymize_text p.baptism_src;
     death = Randomizer.random_death p.death;
     death_place = anonymize_loc ();
     death_note = anonymize_text p.death_note;
     death_src = anonymize_text p.death_src;
     burial = Randomizer.random_burial p.burial;
     burial_place = anonymize_loc ();
     burial_note = anonymize_text p.burial_note;
     burial_src = anonymize_text p.burial_src;
     pevents = List.map anonymize_pevent p.pevents;
     notes = anonymize_text p.notes;
     psources = anonymize_text p.psources
    } in
  Gwdb_driver.patch_person !base p.key_index new_person

let anonymize_family (fam : D.family) =
  let f = D.gen_family_of_family fam in
  let new_fam = {
    f with
    marriage = Randomizer.random_date f.marriage;
    marriage_place = anonymize_loc ();
    marriage_note = anonymize_text f.marriage_note;
    marriage_src = anonymize_text f.marriage_src;
    (* witnesses *)
    (* relation *)
    divorce = Randomizer.random_divorce ();
    fevents = List.map anonymize_fevent f.fevents;
    comment = anonymize_text f.comment;
    (* origin_file *)
    (* fsources *)
    (* fam_index *)
  } in
  Gwdb_driver.patch_family !base f.fam_index new_fam

let anonymize_file (file : string) =
  let content =
    let f = open_in file in
    let res = ref "" in
    try
      while true do
        res := !res ^ " " ^ input_line f;
      done;
      assert false;
    with End_of_file -> close_in f; !res
  in
  let new_content = Randomizer.randomize_wiki_text content in
  let () =
    let f = open_out file in
    let fmt = Format.formatter_of_out_channel f in
    Format.fprintf fmt "%s" new_content
  in
  Format.printf "File %s anonymized@." file

let anonymize_dir (dir : string) =
  try
    let full_dir = Filename.concat !base_name dir in
    if Sys.is_directory full_dir
    then
      let files = Sys.readdir dir in
      let size = Array.length files in
      let rec loop i =
        if i = size then ()
        else if Sys.is_directory files.(i)
        then loop (i+1)
        else
          begin
            anonymize_file files.(i);
            loop (i+1)
          end
      in loop 0
    else Format.printf "No notes in %s@." full_dir
  with Sys_error s ->
    Format.eprintf "Warning while anonymizing dir %s: %s@." dir s


let anonymize () =
  Format.printf "Anonymizing persons@.";
  let persons = D.persons !base in
  D.Collection.iter anonymize_person persons;

  Format.printf "Anonymizing families@.";
  let families = D.families !base in
  D.Collection.iter anonymize_family families;

  Format.printf "Anonymizing base extended notes@.";
  anonymize_dir (D.base_notes_dir !base);

  Format.printf "Anonymizing wizard notes@.";
  anonymize_dir (D.base_wiznotes_dir !base);

  Format.printf "Commiting patches@.";
  D.commit_patches !base

let anon_fun bname =
  let bname = bname ^ ".gwb" in
  check_base_exists bname;
  Random.self_init ();
  anonymize ();
  Gwdb_driver.close_base !base

let arg_usage =
  Printf.sprintf "anon.exe [OPTIONS] TREE\n%s"
    ( String.concat "\n"
        [
          "Anonymize a Geneweb tree.";
          "The following OPTIONS are available:";
        ]
    )

let () =
  let arg_list = [] in
  Arg.parse
    arg_list
    anon_fun
    arg_usage;
  exit 0
