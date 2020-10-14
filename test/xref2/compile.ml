type empty = |

let never_returns : empty -> 'a = function _ -> .

let die =
  Printf.ksprintf (fun s ->
      prerr_endline s;
      (exit 1 :> empty))

let run_command cmd args =
  let r = Sys.command (Filename.quote_command cmd args) in
  if r <> 0 then never_returns (die "Command failed: %s" cmd)

let cmt_filename f =
  let b = Filename.remove_extension f in
  match Filename.extension f with
  | ".ml" -> b ^ ".cmt"
  | ".mli" -> b ^ ".cmti"
  | _ -> never_returns (die "Don't know what to do with '%s'" f)

let odoc_filename f = Filename.remove_extension f ^ ".odoc"

let () =
  let input_files = List.tl (Array.to_list Sys.argv) in
  let odoc_files = List.map odoc_filename input_files in
  let cmt_files = List.map cmt_filename input_files in
  List.iter
    (fun f -> run_command "ocamlc" [ "-bin-annot"; "-c"; f ])
    input_files;
  List.iter
    (fun f -> run_command "odoc" [ "compile"; "--package"; "test"; f ])
    cmt_files;
  List.iter (fun f -> run_command "odoc" [ "link"; "-I"; "."; f ]) odoc_files
