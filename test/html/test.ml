let cases_directory = "cases"
let build_directory = "_scratch"
let test_package = "test_package"
let odoc = "../../src/odoc/bin/main.exe"
let test_source = "test/html"

let (//) = Filename.concat
let command label =
  Printf.ksprintf (fun s ->
    let exit_code = Sys.command s in
    if exit_code <> 0 then
      Alcotest.failf "'%s' exited with %i" label exit_code)

let () =
  let cases : string list =
    Sys.readdir cases_directory
    |> Array.to_list
    |> List.filter (fun entry -> not (Filename.check_suffix entry ".html"))
  in

  Unix.mkdir build_directory 0o755;

  let make_html_test : string -> unit Alcotest.test = fun case_filename ->
    (* Titles. *)
    let file_title = Filename.chop_extension case_filename in
    let test_name = file_title in
    let module_name = String.capitalize_ascii test_name in

    (* Source and intermediate files. *)
    let source_file = cases_directory // case_filename in
    let cmi_file = build_directory // (file_title ^ ".cmi") in
    let cmti_file = build_directory // (file_title ^ ".cmti") in
    let odoc_file = build_directory // (file_title ^ ".odoc") in

    (* HTML files to be compared. *)
    let reference_file = cases_directory // (file_title ^ ".html") in
    let html_file =
      build_directory // test_package // module_name // "index.html" in

    (* Running the actual commands for the test. *)
    let run_test_case () =
      command "ocamlfind c"
        "ocamlfind c -bin-annot -o %s -c %s" cmi_file source_file;

      command "odoc compile"
        "%s compile --package %s %s" odoc test_package cmti_file;

      command "odoc html"
        "%s html --output-dir %s %s" odoc build_directory odoc_file;

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
        let pretty_print_html source_file pretty_printed_file =
          Soup.read_file source_file
          |> Soup.parse
          |> Soup.pretty_print
          |> Soup.write_file pretty_printed_file
        in

        pretty_print_html reference_file pretty_reference_file;
        pretty_print_html html_file pretty_actual_file;

        (* The second diff, of pretty-printed HTML output. *)
        Printf.sprintf "diff -u %s %s" pretty_reference_file pretty_actual_file
        |> Sys.command
        |> ignore;

        (* Also provide the command for overwriting the expected output with the
           actual output, in case it is the actual output that is correct. *)
        prerr_endline "\nTo replace expected output with actual, run";
        Printf.eprintf "cp %s %s\n\n"
          ("_build/default" // test_source // html_file)
          (test_source // reference_file);

        Alcotest.fail "actual HTML output does not match expected"

      | exit_code ->
        Alcotest.failf "'diff' exited with %i" exit_code
    in

    "html", [test_name, `Slow, run_test_case]
  in

  let html_tests = List.map make_html_test cases in

  let output_css : unit Alcotest.test =
    let run_test_case () =
      command "odoc css"
        "%s css --output-dir %s" odoc build_directory
    in

    "css", ["css", `Slow, run_test_case]
  in

  Alcotest.run "html" (output_css::html_tests)
