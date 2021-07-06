type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

type enabledif = Min of string | Max of string | MinMax of string * string

let cu_target_rule enabledif dep_path target_path =
  List
    ([
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
    @ enabledif)

let odoc_target_rule enabledif dep_path target_path =
  List
    ([
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
               Atom "-o";
               Atom "%{target}";
               Atom "%{deps}";
             ];
         ];
     ]
    @ enabledif)

let odocl_target_rule enabledif dep_path target_path =
  List
    ([
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
    @ enabledif)

let mld_odoc_target_rule enabledif dep_path target_path =
  List
    ([
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
               Atom "-o";
               Atom "%{target}";
               Atom "%{deps}";
             ];
         ];
     ]
    @ enabledif)

let set_odocl_ext = Fpath.set_ext ".odocl"

let set_odoc_ext = Fpath.set_ext ".odoc"

let file_rule enabledif path ext =
  let cm_file = Fpath.set_ext ext path in
  let odoc_file = set_odoc_ext path in
  let odocl_file = set_odocl_ext path in
  [
    cu_target_rule enabledif path (Fpath.basename cm_file);
    odoc_target_rule enabledif cm_file odoc_file;
    odocl_target_rule enabledif odoc_file odocl_file;
  ]

let mld_file_rule enabledif path =
  let path' = Fpath.(v ("page-" ^ basename path)) in
  let odoc_file = set_odoc_ext path' in
  let odocl_file = set_odocl_ext path' in
  [
    mld_odoc_target_rule enabledif path odoc_file;
    odocl_target_rule enabledif odoc_file odocl_file;
  ]

let die s =
  prerr_endline s;
  exit 1

let path' () f = Filename.quote (Fpath.to_string f)

let ext' () f = Filename.quote (Fpath.get_ext f)

let cases = Fpath.v "cases"

let is_dot_ocamlformat p = Fpath.filename p = ".ocamlformat"

let gen_rule_for_source_file enabledif path =
  let ext = Fpath.get_ext path in
  match ext with
  | ".ml" -> file_rule enabledif path ".cmt"
  | ".mli" -> file_rule enabledif path ".cmti"
  | ".mld" -> mld_file_rule enabledif path
  | _ ->
      die
        (Printf.sprintf
           "Don't know what to do with %a because of unrecognized %a extension."
           path' path ext' path)

let odocls backend p =
  let path = Fpath.relativize ~root:backend p in
  match path with Some p -> p | None -> assert false

let read_lines ic =
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    assert false
  with End_of_file -> List.rev !lines

let lines_of_file path =
  let ic = open_in (Fpath.to_string path) in
  let lines = read_lines ic in
  close_in ic;
  lines

let create_targets_file f = Fpath.(base f |> set_ext ".targets")

let get_path_to_targets_file backend f =
  create_targets_file f |> Fpath.to_string |> Fpath.( / ) backend

let expected_targets backend test_case =
  let targets_file = get_path_to_targets_file backend test_case in
  try lines_of_file targets_file with _ -> []

let expected_targets' backend test_case =
  expected_targets backend test_case |> List.map (fun t -> Atom (t ^ ".gen"))

let gen_targets_file enabledif ?flat_flag backend target_path path =
  let flat_flag = match flat_flag with None -> [] | Some x -> [ Atom x ] in
  List
    [
      Atom "subdir";
      Atom (Fpath.to_string backend);
      List
        ([
           Atom "rule";
           List
             [
               Atom "action";
               List
                 [
                   Atom "with-outputs-to";
                   Atom Fpath.(to_string (set_ext ".gen" target_path));
                   List
                     ([
                        Atom "run";
                        Atom "odoc";
                        Atom (Fpath.to_string backend ^ "-targets");
                        Atom "-o";
                        Atom ".";
                        Atom ("%{dep:" ^ Fpath.to_string path ^ "}");
                      ]
                     @ flat_flag);
                 ];
             ];
         ]
        @ enabledif);
    ]

let target_diff_rule enabledif backend path =
  let p = Fpath.( // ) backend path in
  List
    ([
       Atom "rule";
       List [ Atom "alias"; Atom "runtest" ];
       List
         [
           Atom "action";
           List
             [
               Atom "diff";
               Atom (Fpath.to_string p);
               Atom Fpath.(to_string (p |> set_ext ".gen"));
             ];
         ];
     ]
    @ enabledif)

let gen_and_diff_target_files_rules enabledif ?flat_flag backend p =
  match flat_flag with
  | Some flat_flag -> (
      let path = Fpath.relativize ~root:backend p in
      match path with
      | Some p ->
          let p' = create_targets_file p in
          [
            gen_targets_file enabledif ~flat_flag backend p' p;
            target_diff_rule enabledif backend p';
          ]
      | None -> [])
  | None -> (
      let path = Fpath.relativize ~root:backend p in
      match path with
      | Some p ->
          let p' = create_targets_file p in
          [
            gen_targets_file enabledif backend p' p;
            target_diff_rule enabledif backend p';
          ]
      | None -> [])

let gen_backend_diff_rule enabledif (b_t_r, b, _) p =
  let p = odocls b p in
  match expected_targets' b p with
  | [] -> []
  | _ ->
      [
        List
          [
            Atom "subdir";
            Atom (Fpath.to_string b);
            List
              ([
                 Atom "rule";
                 List (Atom "targets" :: expected_targets' b p);
                 b_t_r p;
               ]
              @ enabledif);
          ];
      ]

let diff_rule enabledif backend t =
  let t' = Fpath.( // ) backend t in
  List
    ([
       Atom "rule";
       List [ Atom "alias"; Atom "runtest" ];
       List
         [
           Atom "action";
           List
             [
               Atom "diff";
               Atom (Fpath.to_string t');
               Atom (Fpath.to_string t' ^ ".gen");
             ];
         ];
     ]
    @ enabledif)

let diff_rule enabledif backend p =
  let t = expected_targets backend p |> List.map Fpath.v in
  List.map (diff_rule enabledif backend) t

let gen_backend_rule enabledif backend_target_rules path =
  List.map
    (fun b_t_r ->
      let _, b, flag = b_t_r in
      match flag with
      | Some flat_flag ->
          [
            gen_backend_diff_rule enabledif b_t_r path;
            diff_rule enabledif b path;
            gen_and_diff_target_files_rules enabledif ~flat_flag b path;
          ]
          |> List.concat
      | None ->
          [
            gen_backend_diff_rule enabledif b_t_r path;
            diff_rule enabledif b path;
            gen_and_diff_target_files_rules enabledif b path;
          ]
          |> List.concat)
    backend_target_rules
  |> List.flatten

let gen_rule enabledif backend_target_rules paths =
  let enabledif v =
    match List.assoc v enabledif with
    | exception Not_found -> []
    | Min v ->
        [
          List
            [
              Atom "enabled_if";
              List [ Atom ">="; Atom "%{ocaml_version}"; Atom v ];
            ];
        ]
    | Max v ->
        [
          List
            [
              Atom "enabled_if";
              List [ Atom "<="; Atom "%{ocaml_version}"; Atom v ];
            ];
        ]
    | MinMax (min, max) ->
        [
          List
            [
              Atom "enabled_if";
              List
                [
                  Atom "and";
                  List [ Atom ">="; Atom "%{ocaml_version}"; Atom min ];
                  List [ Atom "<="; Atom "%{ocaml_version}"; Atom max ];
                ];
            ];
        ]
  in
  let paths' =
    List.map
      (fun origp ->
        let path = Fpath.relativize ~root:cases origp in
        match path with
        | Some p ->
            let odocl =
              if Fpath.get_ext p = ".mld" then
                set_odocl_ext Fpath.(parent p / ("page-" ^ filename p))
              else set_odocl_ext Fpath.(parent p / filename p)
            in
            (origp, odocl, enabledif p)
        | None -> assert false)
      paths
  in
  List.concat
    [
      List.(
        concat
          (map
             (fun (path, _, enabledif) ->
               gen_rule_for_source_file enabledif path)
             paths'));
      List.map
        (fun (_, p, enabledif) ->
          gen_backend_rule enabledif backend_target_rules p)
        paths'
      |> List.flatten;
    ]
