let () =
  let paths =
    Gen_link_lib.read_file_from_dir (Fpath.filename Gen_link_lib.cases)
  in
  let paths =
    List.filter (fun p -> not (Gen_link_lib.is_dot_ocamlformat p)) paths
  in
  let stanzas = Gen_link_lib.gen_rule paths "4.10" in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
