let die s =
  prerr_endline s;
  exit 1

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

(** Returns filenames, not paths. *)
let read_files_from_dir dir =
  let arr = Sys.readdir (Fpath.to_string dir) in
  Array.sort String.compare arr;
  Array.to_list arr

let constraints =
  let open Gen_rules_lib in
  [
    ("stop_dead_link_doc.mli", Min "4.04");
    ("bugs_post_406.mli", Min "4.06");
    ("ocamlary.mli", Min "4.07");
    ("recent.mli", Min "4.09");
    ("labels.mli", Min "4.09");
    ("recent_impl.ml", Min "4.09");
    ("bugs_pre_410.ml", Max "4.09");
    ("module_type_subst.mli", Min "4.13");
  ]

let test_cases_dir = Fpath.v "cases"

(** Make a test cases or return the empty list if the given file should be
    ignored. Might abort the program with an error. *)
let make_test_case case_name =
  let input = Fpath.(/) test_cases_dir case_name in
  let mk odoc_prefix cmt_suffix =
    let base_out_path = Fpath.v (odoc_prefix ^ case_name) in
    let cmt =
      match cmt_suffix with
      | Some suf -> Some (Fpath.set_ext suf base_out_path)
      | None -> None
    in
    let odoc = Fpath.set_ext ".odoc" base_out_path in
    let odocl = Fpath.set_ext ".odocl" base_out_path in
    let enabledif =
      try Some (List.assoc case_name constraints) with Not_found -> None
    in
    { Gen_rules_lib.input; cmt; odoc; odocl; enabledif }
  in
  match Fpath.get_ext input with
  | ".ml" -> [ mk "" (Some ".cmt") ]
  | ".mli" -> [ mk "" (Some ".cmti") ]
  | ".mld" -> [ mk "page-" None ]
  (* Dune creates directories starting with a dot, which result in an empty
     extension with Fpath. Also, there's [.ocamlformat]. *)
  | "" -> []
  | ext ->
      die
        (Format.asprintf
           "Don't know what to do with %a because of unrecognized %s extension."
           Fpath.pp input ext)

let read_test_cases () =
  read_files_from_dir test_cases_dir |> List.map make_test_case |> List.concat

let () =
  let cases = read_test_cases () in
  let stanzas =
    Gen_rules_lib.gen_rule
      [
        (html_target_rule, Fpath.v "html", Some "--flat");
        (latex_target_rule, Fpath.v "latex", None);
        (man_target_rule, Fpath.v "man", None);
      ]
      cases
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
