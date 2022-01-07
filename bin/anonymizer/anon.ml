module D = Gwdb_driver

let base = ref @@ Obj.magic 0

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
     (* pevents *)
     notes = anonymize_text p.notes;
     psources = anonymize_text p.psources
    } in
  Gwdb_driver.patch_person !base p.key_index new_person

let anonymize () =
  let persons = D.persons !base in
  D.Collection.iter anonymize_person persons;
  D.commit_patches !base

let main () =
  Random.self_init ();
  let b = Gwdb_driver.open_base "a.gwb" in
  base := b;
  anonymize ();
  Gwdb_driver.close_base b

let () = main ()
