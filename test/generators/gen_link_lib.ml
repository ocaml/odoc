type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

let cu_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom target_path ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "ocamlc";
              Atom "-c";
              Atom "-bin-annot";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let odoc_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.basename dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "compile";
              Atom "--pkg";
              Atom "test";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let odocl_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.basename dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "link";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let mld_odoc_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "compile";
              Atom "--pkg";
              Atom "test";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let set_odocl_ext = Fpath.set_ext ".odocl"

let set_odoc_ext = Fpath.set_ext ".odoc"

let file_rule path ext =
  let cm_file = Fpath.set_ext ext path in
  let odoc_file = set_odoc_ext path in
  let odocl_file = set_odocl_ext path in
  [
    cu_target_rule path (Fpath.basename cm_file);
    odoc_target_rule cm_file odoc_file;
    odocl_target_rule odoc_file odocl_file;
  ]

let mld_file_rule path =
  let path' = Fpath.(v ("page-" ^ basename path)) in
  let odoc_file = set_odoc_ext path' in
  let odocl_file = set_odocl_ext path' in
  [
    mld_odoc_target_rule path odoc_file; odocl_target_rule odoc_file odocl_file;
  ]

let die s =
  prerr_endline s;
  exit 1

let path' () f = Filename.quote (Fpath.to_string f)

let ext' () f = Filename.quote (Fpath.get_ext f)

let cases = Fpath.v "cases"

let is_dot_ocamlformat p = Fpath.filename p = ".ocamlformat"

let gen_rule_for_source_file path =
  let ext = Fpath.get_ext path in
  match ext with
  | ".ml" -> file_rule path ".cmt"
  | ".mli" -> file_rule path ".cmti"
  | ".mld" -> mld_file_rule path
  | _ ->
      die
        (Printf.sprintf
           "Don't know what to do with %a because of unrecognized %a extension."
           path' path ext' path)

let html, latex, man = ("html", "latex", "man")

let dune_inc, dune_inc_gen, gen_, exe =
  (".dune.inc", ".dune.inc.gen", "gen_", ".exe")

type backend = {
  subdir : Fpath.t;
  dune_inc : string;
  dune_inc_gen : string;
  dune_inc' : string;
  dune_inc_gen' : string;
  gen_exe : string;
}

let html =
  {
    subdir = Fpath.v html;
    dune_inc = html ^ dune_inc;
    dune_inc_gen = html ^ dune_inc_gen;
    dune_inc' = html ^ "/" ^ html ^ dune_inc;
    dune_inc_gen' = html ^ "/" ^ html ^ dune_inc_gen;
    gen_exe = gen_ ^ html ^ "/" ^ gen_ ^ html ^ exe;
  }

let latex =
  {
    subdir = Fpath.v latex;
    dune_inc = latex ^ dune_inc;
    dune_inc_gen = latex ^ dune_inc_gen;
    dune_inc' = latex ^ "/" ^ latex ^ dune_inc;
    dune_inc_gen' = latex ^ "/" ^ latex ^ dune_inc_gen;
    gen_exe = gen_ ^ latex ^ "/" ^ gen_ ^ latex ^ exe;
  }

let man =
  {
    subdir = Fpath.v man;
    dune_inc = man ^ dune_inc;
    dune_inc_gen = man ^ dune_inc_gen;
    dune_inc' = man ^ "/" ^ man ^ dune_inc;
    dune_inc_gen' = man ^ "/" ^ man ^ dune_inc_gen;
    gen_exe = gen_ ^ man ^ "/" ^ gen_ ^ man ^ exe;
  }

let backends = [ html; latex; man ]

let dep_atom p = Atom (Printf.sprintf "%%{dep:%s}" (Fpath.to_string p))

let odocls backend paths =
  paths
  |> List.map (fun p ->
         let path = Fpath.relativize ~root:backend p in
         match path with Some p -> dep_atom p | None -> assert false)

let gen_backend_diff_rule paths =
  List.map
    (fun b ->
      List
        [
          Atom "subdir";
          Atom (Fpath.to_string b.subdir);
          List
            [
              Atom "rule";
              List
                [
                  Atom "with-stdout-to";
                  Atom b.dune_inc_gen;
                  List
                    [
                      Atom "pipe-stdout";
                      List
                        (Atom "run" :: Atom b.gen_exe :: odocls b.subdir paths);
                      List [ Atom "run"; Atom "dune"; Atom "format-dune-file" ];
                    ];
                ];
            ];
        ])
    backends

let diff_rules ocaml_ver =
  List.map
    (fun b ->
      List
        [
          Atom "rule";
          List [ Atom "alias"; Atom "runtest" ];
          List
            [
              Atom "action";
              List [ Atom "diff"; Atom b.dune_inc'; Atom b.dune_inc_gen' ];
            ];
          List
            [
              Atom "enabled_if";
              List [ Atom ">="; Atom "%{ocaml_version}"; Atom ocaml_ver ];
            ];
        ])
    backends

let gen_backend_rule paths ocaml_ver =
  [ gen_backend_diff_rule paths; diff_rules ocaml_ver ] |> List.flatten

let read_file_from_dir dir =
  let filenames =
    let arr = Sys.readdir dir in
    Array.sort String.compare arr;
    Array.to_list arr
  in
  let dir = Fpath.v dir in
  List.map (Fpath.( / ) dir) filenames

let gen_rule paths ocaml_ver =
  let paths' =
    List.map
      (fun p ->
        let path = Fpath.relativize ~root:cases p in
        match path with
        | Some p ->
            if Fpath.get_ext p = ".mld" then
              set_odocl_ext Fpath.(parent p / ("page-" ^ filename p))
            else set_odocl_ext Fpath.(parent p / filename p)
        | None -> assert false)
      paths
  in
  List.flatten
    [
      List.(flatten (map gen_rule_for_source_file paths));
      gen_backend_rule paths' ocaml_ver;
    ]
