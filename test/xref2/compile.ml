let die s =
  prerr_endline s;
  exit 1

let spf = Printf.sprintf

let run =
  Printf.ksprintf (fun cmd ->
      let r = Sys.command cmd in
      if r <> 0 then die (spf "Command failed: %s" cmd))

let path () f = Filename.quote (Fpath.to_string f)

let cmt_filename f =
  match Fpath.get_ext f with
  | ".ml" -> Fpath.set_ext ".cmt" f
  | ".mli" -> Fpath.set_ext ".cmti" f
  | _ -> die (spf "Don't know what to do with %a" path f)

let odoc_filename f = Fpath.set_ext ".odoc" f

let () =
  let input_files = List.map Fpath.v (List.tl (Array.to_list Sys.argv)) in
  let cmt_files = List.map cmt_filename input_files in
  let odoc_files = List.map odoc_filename input_files in
  List.iter (run "ocamlc -bin-annot -c %a" path) input_files;
  List.iter (run "odoc compile --package test %a" path) cmt_files;
  List.iter (run "odoc link -I . %a" path) odoc_files
