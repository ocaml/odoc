let html_target_rule path : Gen_rules_lib.sexp =
  List
    [
      Atom "action";
      List
        [
          Atom "progn";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "html-generate";
              Atom "--indent";
              Atom "--flat";
              Atom "--extra-suffix";
              Atom "gen";
              Atom "-o";
              Atom ".";
              Atom ("%{dep:" ^ Fpath.to_string path ^ "}");
            ];
        ];
    ]

let latex_target_rule path : Gen_rules_lib.sexp =
  List
    [
      Atom "action";
      List
        [
          Atom "progn";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "latex-generate";
              Atom "-o";
              Atom ".";
              Atom "--extra-suffix";
              Atom "gen";
              Atom ("%{dep:" ^ Fpath.to_string path ^ "}");
            ];
        ];
    ]

let man_target_rule path : Gen_rules_lib.sexp =
  List
    [
      Atom "action";
      List
        [
          Atom "progn";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "man-generate";
              Atom "-o";
              Atom ".";
              Atom "--extra-suffix";
              Atom "gen";
              Atom ("%{dep:" ^ Fpath.to_string path ^ "}");
            ];
        ];
    ]

let read_file_from_dir dir =
  let filenames =
    let arr = Sys.readdir dir in
    Array.sort String.compare arr;
    Array.to_list arr
  in
  let dir = Fpath.v dir in
  List.map (Fpath.( / ) dir) filenames

let constraints =
  let open Gen_rules_lib in
  [
    (Fpath.v "stop_dead_link_doc.mli", Min "4.04");
    (Fpath.v "bugs_post_406.mli", Min "4.06");
    (Fpath.v "ocamlary.mli", Min "4.07");
    (Fpath.v "recent.mli", Min "4.09");
    (Fpath.v "labels.mli", Min "4.09");
    (Fpath.v "recent_impl.ml", Min "4.09");
    (Fpath.v "bugs_pre_410.ml", Max "4.09");
    (Fpath.v "module_type_subst.mli", Min "4.13");
  ]

let () =
  let paths = read_file_from_dir (Fpath.filename Gen_rules_lib.cases) in
  let paths =
    List.filter (fun p -> not (Gen_rules_lib.is_dot_ocamlformat p)) paths
  in
  let stanzas =
    Gen_rules_lib.gen_rule constraints
      [
        (html_target_rule, Fpath.v "html", Some "--flat");
        (latex_target_rule, Fpath.v "latex", None);
        (man_target_rule, Fpath.v "man", None);
      ]
      paths
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
