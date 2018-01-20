let cases_directory = "cases"
let expect_directory = "expect"
let build_directory = "_scratch"
let test_package = "test_package"
let odoc = "../../src/odoc/bin/main.exe"
let test_root = "test/html"

let (//) = Filename.concat

let contains_actual = build_directory // "actual"
let contains_expected = build_directory // "expected"

let command label =
  Printf.ksprintf (fun s ->
    let exit_code = Sys.command s in
    if exit_code <> 0 then
      Alcotest.failf "'%s' exited with %i" label exit_code)



let cases = [
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

let () =
  Unix.mkdir build_directory 0o755;

  let already_failed = ref false in

  let make_html_test : string -> unit Alcotest.test_case = fun case_filename ->
    (* Titles. *)
    let file_title = Filename.chop_extension case_filename in
    let test_name = file_title in
    let module_name = String.capitalize_ascii test_name in

    (* Filename.extension is only available on 4.04. *)
    let extension =
      let dot_index = String.rindex case_filename '.' in
      String.sub
        case_filename dot_index (String.length case_filename - dot_index)
    in

    (* Source and intermediate files. *)
    let source_file = cases_directory // case_filename in
    let cmi_file = build_directory // (file_title ^ ".cmi") in
    let cmti_file = build_directory // (file_title ^ ".cmti") in
    let odoc_file = build_directory // (file_title ^ ".odoc") in

    (* HTML files to be compared. *)
    let reference_file = expect_directory // (file_title ^ ".html") in
    let html_file =
      match extension with
      | ".mli" -> build_directory // test_package // module_name // "index.html"
      | ".mld" -> build_directory // test_package // "index.html"
      | _ -> assert false
    in

    (* The commands to run differ depending on what kind of source filw we
       have. *)
    let generate_html =
      match extension with
      | ".mli" ->
        fun () ->
          command "ocamlfind c"
            "ocamlfind c -bin-annot -o %s -c %s" cmi_file source_file;

          command "odoc compile"
            "%s compile --package %s %s" odoc test_package cmti_file;

          command "odoc html"
            "%s html --output-dir %s %s" odoc build_directory odoc_file;

      | ".mld" ->
        fun () ->
          prerr_endline (Sys.getcwd ());
          prerr_endline case_filename;
          command "odoc html"
            "%s html --output-dir %s --index-for %s %s"
            odoc build_directory test_package source_file

      | _ ->
        assert false
    in

    (* Running the actual commands for the test. *)
    let run_test_case () =
      generate_html ();

      let diff = Printf.sprintf "diff -u %s %s" reference_file html_file in
      match Sys.command diff with
      | 0 ->
        ()

      | 1 ->
        (* If the diff command exits with 1, the two HTML files are different.
           diff has already written its output to STDOUT, but depending on the
           formatting of the HTML files, it might be illegible. To create a
           legible diff for the human reading the test output, pretty-print both
           HTML files, and diff the pretty-printed output. *)
        prerr_endline "\nPretty-printed diff:\n";

        (* The filenames to use. *)
        let pretty_reference_file =
          build_directory // "reference.pretty.html" in
        let pretty_actual_file =
          build_directory // "actual.pretty.html" in

        (* Do the actual pretty printing. *)
        pretty_print_html reference_file pretty_reference_file;
        pretty_print_html html_file pretty_actual_file;

        (* The second diff, of pretty-printed HTML output. *)
        Printf.sprintf "diff -u %s %s" pretty_reference_file pretty_actual_file
        |> Sys.command
        |> ignore;

        (* Also provide the command for overwriting the expected output with the
           actual output, in case it is the actual output that is correct. *)
        if not !already_failed then begin
          let actual = "_build/default" // test_root // html_file in
          let expected = test_root // reference_file in
          Soup.write_file contains_actual actual;
          Soup.write_file contains_expected expected;

          let actual = "_build/default" // test_root // contains_actual in
          let expected = "_build/default" // test_root // contains_expected in
          prerr_endline "\nTo replace expected output with actual, run";
          Printf.eprintf
            "cp `cat %s` `cat %s` && make test\n\n" actual expected;

          already_failed := true
        end;

        Alcotest.fail "actual HTML output does not match expected"

      | exit_code ->
        Alcotest.failf "'diff' exited with %i" exit_code
    in

    test_name, `Slow, run_test_case
  in

  let html_tests : unit Alcotest.test = "html", List.map make_html_test cases in

  let output_assets : unit Alcotest.test =
    let run_test_case () =
      command "odoc css"
        "%s css --output-dir %s" odoc build_directory
    in

    "assets", ["assets", `Slow, run_test_case]
  in

  Alcotest.run "html" [output_assets; html_tests]
