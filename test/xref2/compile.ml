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

let odoc_filename f =
  let open Fpath in
  let f =
    if get_ext f = ".mld" then add_seg (parent f) ("page-" ^ basename f) else f
  in
  set_ext ".odoc" f

let () =
  let input_files = List.map Fpath.v (List.tl (Array.to_list Sys.argv)) in
  let mld_files, ocaml_files =
    List.partition (fun f -> Fpath.get_ext f = ".mld") input_files
  in
  let cmt_files =
    let mli_files =
      List.fold_left
        (fun acc f ->
          if Fpath.has_ext ".mli" f then Fpath.rem_ext f :: acc else acc)
        [] ocaml_files
    in
    ocaml_files
    (* Remove .ml files that have a corresponding .mli. *)
    |> List.filter (fun f ->
           not (Fpath.has_ext ".ml" f && List.mem (Fpath.rem_ext f) mli_files))
    (* Side effect: error on unknown extension *)
    |> List.map cmt_filename
  in
  let odoc_files = List.map odoc_filename input_files in
  List.iter (run "ocamlc -bin-annot -c %a" path) ocaml_files;
  List.iter
    (run "odoc compile -I . --package test %a" path)
    (mld_files @ cmt_files);
  List.iter (run "odoc link -I . %a" path) odoc_files
