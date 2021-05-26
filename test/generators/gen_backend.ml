type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

let read_lines ic =
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    assert false
  with End_of_file -> List.rev !lines

let die s =
  prerr_endline s;
  exit 1

let lines_of_command prog args =
  let cmd = String.concat " " (prog :: List.map Filename.quote args) in
  let inp = Unix.open_process_in cmd in
  let lines = read_lines inp in
  match Unix.close_process_in inp with
  | Unix.WEXITED 0 -> lines
  | _ -> die (cmd ^ " exited with non-zero status")

let expected_targets backend odocl =
  lines_of_command "odoc "
    [ backend ^ "-targets"; "-o"; backend ^ ".gen"; Fpath.to_string odocl ]
  |> List.map Fpath.v

let tweak_target target =
  match Fpath.segs target with
  | _ :: _ :: rest -> String.concat "." rest
  | _ -> assert false

let gen_targets targets =
  List.map
    (fun t ->
      List
        [
          Atom "with-stdout-to";
          Atom (tweak_target t ^ ".gen");
          List
            [
              Atom "progn";
              List
                [
                  Atom "system";
                  Atom ("cat " ^ Filename.quote (Fpath.to_string t));
                ];
            ];
        ])
    targets

let diff_rule t ocaml_ver =
  List
    [
      Atom "rule";
      List [ Atom "alias"; Atom "runtest" ];
      List
        [
          Atom "action";
          List
            [
              Atom "diff";
              Atom (Fpath.to_string t);
              Atom (Fpath.to_string t ^ ".gen");
            ];
        ];
      List
        [
          Atom "enabled_if";
          List [ Atom ">="; Atom "%{ocaml_version}"; Atom ocaml_ver ];
        ];
    ]

let diff_rules targets ocaml_ver =
  List.map (fun t -> diff_rule t ocaml_ver) targets

let gen_backend_rules backend target_rule filenames ocaml_ver =
  let rules =
    (List.map (fun odocl ->
         let targets = expected_targets backend odocl in
         let paths = List.map tweak_target targets |> List.map Fpath.v in
         target_rule odocl targets :: diff_rules paths ocaml_ver))
      filenames
  in
  List.flatten rules

let files = List.tl (Array.to_list Sys.argv) |> List.map Fpath.v
