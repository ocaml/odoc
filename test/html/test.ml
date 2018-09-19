open Compat
open Printf

(* Utils *)

let (//) = Filename.concat

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
    | `scratch when from_root -> "_build/default/test/html/_scratch"
    | `scratch -> "_scratch"
    | `expect when from_root -> "test/html/expect"
    | `expect -> "expect"
    | `cases when from_root -> "test/html/cases"
    | `cases -> "cases"

  let running_in_travis =
    match Sys.getenv "TRAVIS" with
    | "true" -> true
    | _ -> false
    | exception Not_found -> false

  let init () = begin
    if running_in_travis && not Tidy.is_present_in_path then begin
      Alcotest.failf "Could not find `tidy` in $PATH in a CI environment"
    end;

    Unix.mkdir (path `scratch) 0o755
  end
end


(* Test case type and helpers *)

(* A test case is a description of an input source file with a specific set of
   options to be tested. Each test case results in a unique generated output to
   be compared with an actually produced one.

   All paths defined in this module are relative to the build directory. *)
module Case = struct
  type t = {
    name: string;
    kind: [ `mld | `mli ];
    theme_uri: string option;
    syntax: [ `ml | `re ];
  }

  let make ?theme_uri ?(syntax = `ml) basename =
    let name = Filename.chop_extension basename in
    let kind =
      match Filename.extension basename with
      | ".mli" -> `mli
      | ".mld" -> `mld
      | _ -> invalid_arg (sprintf "Expected mli or mld files, got %s" basename)
    in
    { name; kind; theme_uri; syntax }

  let name case = case.name
  let kind case = case.kind
  let theme_uri case = case.theme_uri

  let string_of_syntax = function
    | `re -> "re"
    | `ml -> "ml"

  (* The package name is enriched with test case options. *)
  let package case =
    let opts = [string_of_syntax case.syntax] in
    let opts =
      match case.theme_uri with
      | Some _ -> "custom_theme" :: opts
      | None -> opts in
    let opts = String.concat "," (List.sort Pervasives.compare opts) in
    Env.package ^ "+" ^ opts

  let cmi_file case  = Env.path `scratch // (case.name ^ ".cmi")
  let cmti_file case = Env.path `scratch // (case.name ^ ".cmti")

  let odoc_file case =
    match case.kind with
    | `mli -> Env.path `scratch // (case.name ^ ".odoc")
    | `mld -> Env.path `scratch // ("page-" ^ case.name ^ ".odoc")

  let source_file case =
    match case.kind with
    | `mli -> Env.path `cases // case.name ^ ".mli"
    | `mld -> Env.path `cases // case.name ^ ".mld"

  (* Produces an HTML file path for a given case, starting with [dir]. *)
  let html_file dir case =
    let module_name = String.capitalize_ascii case.name in
    match case.kind with
    | `mli -> dir // package case // module_name // "index.html"
    | `mld -> dir // package case // case.name ^ ".html"

  let actual_html_file   ?from_root = html_file (Env.path ?from_root `scratch)
  let expected_html_file ?from_root = html_file (Env.path ?from_root `expect)
end


let pretty_print_html source_file pretty_printed_file =
  Markup.file source_file
  |> fst
  |> Markup.parse_html
  |> Markup.signals
  |> Markup.transform begin fun (at_start_of_line, after_markup) signal ->
    match signal with
    | `Text ss ->
      (* Markup.ml gives a "normalized" signal stream, so no empty text
         signals. *)
      let s = String.concat "" ss in
      let at_start_of_line = s.[String.length s - 1] = '\n' in
      let s =
        if after_markup && s.[0] = '\n' then
          String.sub s 1 (String.length s - 1)
        else
          s
      in
      [`Text [s]], Some (at_start_of_line, false)

    | _ ->
      let signals =
        if at_start_of_line then
          [signal; `Text ["\n"]]
        else
          [`Text ["\n"]; signal; `Text ["\n"]]
      in
      signals, Some (true, true)
  end (true, false)
  |> Markup.write_html
  |> Markup.to_file pretty_printed_file


let generate_html case =
  let theme_uri_option =
    match Case.theme_uri case with
    | Some theme_uri -> "--theme-uri=" ^ theme_uri
    | None -> ""
  in
  match Case.kind case with
  | `mli ->
    command "ocamlfind c" "ocamlfind c -bin-annot -o %s -c %s"
      (Case.cmi_file case) (Case.source_file case);

    command "odoc compile" "%s compile --package=%s %s"
      Env.odoc (Case.package case) (Case.cmti_file case);

    command "odoc html" "%s html %s --syntax=%s --output-dir=%s %s"
      Env.odoc theme_uri_option (Case.string_of_syntax case.syntax)
      (Env.path `scratch) (Case.odoc_file case)

  | `mld ->
    command "odoc compile" "%s compile --package=%s -o %s %s"
      Env.odoc (Case.package case) (Case.odoc_file case) (Case.source_file case);

    command "odoc html" "%s html %s --output-dir=%s %s"
      Env.odoc theme_uri_option (Env.path `scratch) (Case.odoc_file case)

let diff =
  (* Alcotest will run all tests. We need to know when something fails for the
     first time to stop diffing and generating promotion files. *)
  let already_failed = ref false in
  fun case ->
    let expected_file = Case.expected_html_file case in
    let actual_file   = Case.actual_html_file case in
    let cmd = sprintf "diff -u %s %s" expected_file actual_file in
    match Sys.command cmd with
    | 0 -> ()

    | 1 when !already_failed ->
      (* Don't run diff for other failing tests as only one at time is shown. *)
      Alcotest.fail "generated HTML should match expected"

    | 1 ->
      (* If the diff command exits with 1, the two HTML files are different.
         diff has already written its output to STDOUT, but depending on the
         formatting of the HTML files, it might be illegible. To create a
         legible diff for the human reading the test output, pretty-print both
         HTML files, and diff the pretty-printed output. *)
      prerr_endline "\nPretty-printed diff:\n";

      (* The filenames to use. *)
      let pretty_expected_file = Env.path `scratch // "expected.pretty.html" in
      let pretty_actual_file   = Env.path `scratch // "actual.pretty.html" in

      (* Do the actual pretty printing. *)
      pretty_print_html expected_file pretty_expected_file;
      pretty_print_html actual_file pretty_actual_file;

      (* The second diff, of pretty-printed HTML output. *)
      let pretty_cmd = sprintf "diff -u %s %s" pretty_expected_file pretty_actual_file in
      ignore (Sys.command pretty_cmd);

      (* Also provide the command for overwriting the expected output with the
         actual output, in case it is the actual output that is correct.
         The paths are defined relative to the project's root. *)
      let root_actual_file   = Case.actual_html_file   ~from_root:true case in
      let root_expected_file = Case.expected_html_file ~from_root:true case in
      Soup.write_file Env.(path `scratch // "actual") root_actual_file;
      Soup.write_file Env.(path `scratch // "expected") root_expected_file;

      prerr_endline "\nTo promote the actual output to expected, run:";
      Printf.eprintf "cp `cat %s` `cat %s` && make test\n\n"
        Env.(path ~from_root:true `scratch // "actual")
        Env.(path ~from_root:true `scratch // "expected");

      already_failed := true;
      Alcotest.fail "generated HTML should match expected"

    | exit_code ->
      Alcotest.failf "'diff' exited with %i" exit_code


(* Actual Tests *)

let output_support_files =
  let run () =
    command "odoc support-files" "%s support-files --output-dir %s"
      Env.odoc (Env.path `scratch)
  in
  "support-files", `Slow, run


let make_test_case ?theme_uri ?syntax case_basename =
  let case = Case.make ?theme_uri ?syntax case_basename in
  let run () =
    (* Compile the source file and generate HTML. *)
    generate_html case;

    (* Run HTML validation *)
    if Tidy.is_present_in_path then begin
      let issues = Tidy.validate (Case.actual_html_file case) in
      if issues <> [] then begin
        List.iter prerr_endline issues;
        Alcotest.fail "Tidy validation error"
      end
    end;

    (* Diff the actual output with the expected output. *)
    diff case;
  in
  Case.name case, `Slow, run


let source_files = [
  "val.mli";
  "markup.mli";
  "section.mli";
  "module.mli";
  "interlude.mli";
  "include.mli";
  "mld.mld";
  "type.mli";
  "external.mli";
  "functor.mli";
  "class.mli";
  "stop.mli";
]


let () =
  Env.init ();

  Alcotest.run "html" [
    "support_files", [output_support_files];
    "html_ml", List.map (make_test_case ~syntax:`ml) source_files;
    "html_re", List.map (make_test_case ~syntax:`re) source_files;
    "custom_theme", [
      make_test_case ~theme_uri:"/a/b/c" "module.mli";
      make_test_case ~theme_uri:"https://foo.com/a/b/c/" "val.mli";
      make_test_case ~theme_uri:"../b/c" "include.mli";
      make_test_case ~theme_uri:"b/c" "section.mli";
    ];
  ]
