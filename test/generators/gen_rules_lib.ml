type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

type enabledif = Min of string | Max of string | MinMax of string * string

type test_case = {
  input : Fpath.t;
  cmt : Fpath.t option;  (** [None] for mld files. *)
  odoc : Fpath.t;
  odocl : Fpath.t;
  enabledif : enabledif option;
}

module Dune = struct
  let arg_fpath f = Fpath.to_string f

  let arg_dep f = "%{dep:" ^ Fpath.to_string f ^ "}"

  let arg_list args = List.map (fun x -> Atom x) args

  let render_enabledif = function
    | Some (Min v) ->
        [
          List
            [
              Atom "enabled_if";
              List [ Atom ">="; Atom "%{ocaml_version}"; Atom v ];
            ];
        ]
    | Some (Max v) ->
        [
          List
            [
              Atom "enabled_if";
              List [ Atom "<="; Atom "%{ocaml_version}"; Atom v ];
            ];
        ]
    | Some (MinMax (min, max)) ->
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
    | None -> []

  let run cmd = List (Atom "run" :: arg_list cmd)

  let action x = List [ Atom "action"; x ]

  let rule ?enabledif fields =
    List ((Atom "rule" :: fields) @ render_enabledif enabledif)

  let simple_rule ?enabledif target cmd =
    rule ?enabledif
      [ List [ Atom "target"; Atom (arg_fpath target) ]; action (run cmd) ]

  let rule_with_output_to ?enabledif target cmd =
    rule ?enabledif
      [
        action
          (List [ Atom "with-outputs-to"; Atom (arg_fpath target); run cmd ]);
      ]

  let runtest_diff ?enabledif file_a file_b =
    rule ?enabledif
      [
        List [ Atom "alias"; Atom "runtest" ];
        action
          (List
             [ Atom "diff"; Atom (arg_fpath file_a); Atom (arg_fpath file_b) ]);
      ]

  let subdir dir rules = List (Atom "subdir" :: Atom (arg_fpath dir) :: rules)
end

let cu_target_rule enabledif dep target =
  Dune.simple_rule ?enabledif target
    [ "ocamlc"; "-c"; "-bin-annot"; "-o"; "%{target}"; Dune.arg_dep dep ]

let odoc_target_rule enabledif dep target =
  Dune.simple_rule ?enabledif target
    [ "odoc"; "compile"; "-o"; "%{target}"; Dune.arg_dep dep ]

let odocl_target_rule enabledif dep target =
  Dune.simple_rule ?enabledif target
    [ "odoc"; "link"; "-o"; "%{target}"; Dune.arg_dep dep ]

let gen_rule_for_source_file { input; cmt; odoc; odocl; enabledif } =
  let cmt_rule, input =
    match cmt with
    | Some cmt -> ([ cu_target_rule enabledif input cmt ], cmt)
    | None -> ([], input)
  in
  cmt_rule
  @ [
      odoc_target_rule enabledif input odoc;
      odocl_target_rule enabledif odoc odocl;
    ]

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

let targets_file_path f = Fpath.(base f |> set_ext ".targets")

let expected_targets backend test_case =
  let targets_file = Fpath.( // ) backend (targets_file_path test_case) in
  try lines_of_file targets_file |> List.map Fpath.v with _ -> []

let gen_targets_file enabledif ?flat_flag backend target_path relinput =
  let flat_flag = match flat_flag with None -> [] | Some x -> [ x ] in
  let gen_path = Fpath.add_ext ".gen" target_path in
  [
    Dune.subdir backend
      [
        Dune.rule_with_output_to ?enabledif gen_path
          ([
             "odoc";
             Fpath.to_string backend ^ "-targets";
             "-o";
             ".";
             Dune.arg_dep relinput;
           ]
          @ flat_flag);
        Dune.runtest_diff ?enabledif target_path gen_path;
      ];
  ]

let gen_backend_diff_rule enabledif ~targets (b_t_r, b, _) p =
  match targets with
  | [] -> []
  | _ ->
      let targets_gen = List.map (Fpath.add_ext ".gen") targets in
      Dune.
        [
          subdir b
            (rule ?enabledif
               [
                 List
                   (Atom "targets"
                   :: List.map (fun t -> Atom (Dune.arg_fpath t)) targets_gen);
                 action (run (b_t_r p));
               ]
            :: List.map2 (Dune.runtest_diff ?enabledif) targets targets_gen);
        ]

let gen_backend_rule enabledif backend_target_rules path =
  List.map
    (fun b_t_r ->
      let _, b, flat_flag = b_t_r in
      let targets = expected_targets b path in
      let relpath =
        let path = Fpath.relativize ~root:b path in
        match path with Some p -> p | None -> assert false
      in
      let targets_file = targets_file_path relpath in
      [
        gen_backend_diff_rule enabledif ~targets b_t_r relpath;
        gen_targets_file enabledif ?flat_flag b targets_file relpath;
      ]
      |> List.concat)
    backend_target_rules
  |> List.flatten

let gen_rule backend_target_rules test_cases =
  List.concat
    [
      List.(concat (map gen_rule_for_source_file test_cases));
      List.map
        (fun case ->
          gen_backend_rule case.enabledif backend_target_rules case.odocl)
        test_cases
      |> List.flatten;
    ]
