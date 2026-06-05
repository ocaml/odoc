let ends_with_newline s = String.length s > 0 && s.[String.length s - 1] = '\n'

let print_to oc ~input ~output =
  output_string oc "--- input ---\n";
  output_string oc input;
  if not (ends_with_newline input) then output_char oc '\n';
  output_string oc "--- output ---\n";
  output_string oc output;
  if not (ends_with_newline output) then output_char oc '\n'

let print ~input ~output = print_to stdout ~input ~output

let expect ~input format_output =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  format_output fmt;
  Format.pp_print_flush fmt ();
  print ~input ~output:(Buffer.contents buf)

let lexing_position ?(filename = "f.ml") ?(line = 1) ?(column = 0) () =
  {
    Lexing.pos_fname = filename;
    pos_lnum = line;
    pos_bol = 0;
    pos_cnum = column;
  }

let emit_rules ?package exe groups =
  let pkg_line =
    match package with
    | Some p -> Printf.sprintf "\n (package %s)" p
    | None -> ""
  in
  List.iter
    (fun (name, _) ->
      Printf.printf
        "(rule\n\
        \ (alias runtest)%s\n\
        \ (action\n\
        \  (progn\n\
        \   (with-stdout-to %s.actual (run %%{exe:%s} %s))\n\
        \   (diff expected/%s.expected %s.actual))))\n\n"
        pkg_line name exe name name name)
    groups

let run ?package groups =
  let exe = Filename.basename Sys.argv.(0) in
  match Sys.argv with
  | [| _; "--gen-rules" |] -> emit_rules ?package exe groups
  | [| _; name |] -> (
      match List.assoc_opt name groups with
      | Some f -> f ()
      | None ->
          prerr_endline ("Unknown group: " ^ name);
          exit 2)
  | _ ->
      prerr_endline (Printf.sprintf "usage: %s <group> | --gen-rules" exe);
      exit 2
