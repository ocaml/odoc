open Printf

(* Utils *)

let ( // ) = Filename.concat

let command label =
  Printf.ksprintf (fun s ->
      let exit_code = Sys.command s in
      if exit_code <> 0 then
        Alcotest.failf "'%s' exited with %i" label exit_code)

(* Filename.extension is only available on 4.04. *)
module Filename = struct
  include Filename

  let extension filename =
    let dot_index = String.rindex filename '.' in
    String.sub filename dot_index (String.length filename - dot_index)
end

(* Testing environment *)

module Env = struct
  let package = "test_package"

  let odoc = "../../src/odoc/bin/main.exe"

  let path ?(from_root = false) = function
    | `scratch when from_root -> "_build/default/test/latex/_scratch"
    | `scratch -> "_scratch"
    | `expect when from_root -> "test/latex/expect"
    | `expect -> "expect"
    | `cases when from_root -> "test/cases"
    | `cases -> "../cases"

  let init () = Unix.mkdir (path `scratch) 0o755
end

(* Test case type and helpers *)

(* A test case is a description of an input source file with a specific set of
   options to be tested. Each test case results in a unique generated output to
   be compared with an actually produced one.

   All paths defined in this module are relative to the build directory. *)
module Case = struct
  type t = {
    name : string;
    kind : [ `mli | `mld | `ml ];
    syntax : [ `ml | `re ];
    outputs : string list;
  }

  let make ?(syntax = `ml) (input, outputs) =
    let name = Filename.chop_extension input in
    let kind =
      match Filename.extension input with
      | ".mli" -> `mli
      | ".mld" -> `mld
      | ".ml" -> `ml
      | _ ->
          invalid_arg (sprintf "Expected mli, mld, or ml files, got %s" input)
    in
    { name; kind; syntax; outputs }

  let name case = case.name

  let kind case = case.kind

  let string_of_syntax = function `re -> "re" | `ml -> "ml"

  (* The package name is enriched with test case options. *)
  let package case =
    let opts = [ string_of_syntax case.syntax ] in
    let opts = String.concat "," (List.sort compare opts) in
    Env.package ^ "+" ^ opts

  let cmi_file case = Env.path `scratch // (case.name ^ ".cmi")

  let cmti_file case = Env.path `scratch // (case.name ^ ".cmti")

  let cmo_file case = Env.path `scratch // (case.name ^ ".cmo")

  let cmt_file case = Env.path `scratch // (case.name ^ ".cmt")

  let odoc_file case =
    match case.kind with
    | `mli | `ml -> Env.path `scratch // (case.name ^ ".odoc")
    | `mld -> Env.path `scratch // ("page-" ^ case.name ^ ".odoc")

  let source_file case =
    match case.kind with
    | `mli -> (Env.path `cases // case.name) ^ ".mli"
    | `mld -> (Env.path `cases // case.name) ^ ".mld"
    | `ml -> (Env.path `cases // case.name) ^ ".ml"

  let outputs case = List.map (fun o -> package case // o) case.outputs
end

let generate_latex case =
  match Case.kind case with
  | `mli ->
      command "ocamlfind c" "ocamlfind c -bin-annot -o %s -c %s"
        (Case.cmi_file case) (Case.source_file case);

      command "odoc compile" "%s compile --package=%s %s" Env.odoc
        (Case.package case) (Case.cmti_file case);

      command "odoc latex" "%s latex --syntax=%s --output-dir=%s %s" Env.odoc
        (Case.string_of_syntax case.syntax)
        (Env.path `scratch)
        (Case.odoc_file case)
  | `mld ->
      command "odoc compile" "%s compile --package=%s -o %s %s" Env.odoc
        (Case.package case) (Case.odoc_file case) (Case.source_file case);

      command "odoc latex" "%s latex --output-dir=%s %s" Env.odoc
        (Env.path `scratch)
        (Case.odoc_file case)
  | `ml ->
      command "ocamlfind c" "ocamlfind c -bin-annot -o %s -c %s"
        (Case.cmo_file case) (Case.source_file case);

      command "odoc compile" "%s compile --package=%s %s" Env.odoc
        (Case.package case) (Case.cmt_file case);

      command "odoc latex" "%s latex --syntax=%s --output-dir=%s %s" Env.odoc
        (Case.string_of_syntax case.syntax)
        (Env.path `scratch)
        (Case.odoc_file case)

let diff =
  (* Alcotest will run all tests. We need to know when something fails for the
     first time to stop diffing and generating promotion files. *)
  let already_failed = ref false in
  fun output ->
    let actual_file = Env.path `scratch // output in
    let expected_file = Env.path `expect // output in
    let cmd = sprintf "diff -N -u -b %s %s" expected_file actual_file in
    match Sys.command cmd with
    | 0 -> ()
    | 1 when !already_failed ->
        (* Don't run diff for other failing tests as only one at time is shown. *)
        Alcotest.fail "generated latex files should match expected"
    | 1 ->
        (* If the diff command exits with 1, the two MAN files are different.
           diff has already written its output to STDOUT.

           Also provide the command for overwriting the expected output with the
           actual output, in case it is the actual output that is correct.
           The paths are defined relative to the project's root. *)
        let root_actual_file = Env.path `scratch ~from_root:true // output in
        let root_expected_file = Env.path `expect ~from_root:true // output in
        let write_file filename data =
          let oc = open_out filename in
          output_string oc data;
          close_out oc
        in
        write_file Env.(path `scratch // "actual") root_actual_file;
        write_file Env.(path `scratch // "expected") root_expected_file;

        prerr_endline "\nTo promote the actual output to expected, run:";
        Printf.eprintf "cp `cat %s` `cat %s` && make test\n\n"
          Env.(path ~from_root:true `scratch // "actual")
          Env.(path ~from_root:true `scratch // "expected");

        already_failed := true;
        Alcotest.fail "generated latex files should match expected"
    | exit_code -> Alcotest.failf "'diff' exited with %i" exit_code

let make_test_case ?syntax case =
  let case = Case.make ?syntax case in
  let run () =
    (* Compile the source file and generate latex files. *)
    generate_latex case;

    List.iter diff (Case.outputs case)
  in
  (Case.name case, `Slow, run)

let source_files_all =
  [
    ("val.mli", [ "Val.tex" ]);
    ("markup.mli", [ "Markup.tex" ]);
    ("section.mli", [ "Section.tex" ]);
    ("module.mli", [ "Module.tex" ]);
    ("interlude.mli", [ "Interlude.tex" ]);
    ("include.mli", [ "Include.tex" ]);
    ("include2.ml", [ "Include2.tex" ]);
    ("include_sections.mli", [ "Include_sections.tex" ]);
    ("mld.mld", [ "mld.tex" ]);
    ( "nested.mli",
      [ "Nested.tex"; "Nested.F.tex"; "Nested.z.tex"; "Nested.inherits.tex" ] );
    ("type.mli", [ "Type.tex" ]);
    ("external.mli", [ "External.tex" ]);
    ( "functor.mli",
      [
        "Functor.tex";
        "Functor.F1.tex";
        "Functor.F2.tex";
        "Functor.F3.tex";
        "Functor.F4.tex";
      ] );
    ("class.mli", [ "Class.tex" ]);
    ("stop.mli", [ "Stop.tex" ]);
    ("bugs.ml", [ "Bugs.tex" ]);
    ("alias.ml", [ "Alias.tex"; "Alias.X.tex" ]);
  ]

let source_files_post408 =
  [
    ("recent.mli", [ "Recent.tex" ]);
    ("recent_impl.ml", [ "Recent_impl.tex"; "Recent_impl.B.tex" ]);
  ]

let source_files_pre410 = [ ("bugs_pre_410.ml", [ "Bugs_pre_410.tex" ]) ]

let source_files =
  let cur =
    Astring.String.cuts ~sep:"." Sys.ocaml_version
    |> List.map (fun i -> try Some (int_of_string i) with _ -> None)
  in
  match cur with
  | Some major :: Some minor :: _ ->
      List.concat
        [
          (if major = 4 && minor < 10 then source_files_pre410 else []);
          (if major = 4 && minor > 8 then source_files_post408 else []);
          source_files_all;
        ]
  | _ -> source_files_all

let () =
  Env.init ();

  Alcotest.run "latex"
    [ ("latex_ml", List.map (make_test_case ~syntax:`ml) source_files) ]
