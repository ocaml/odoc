#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let (!) fmt = Printf.sprintf fmt

let rec stop_at_first_error lst ~f =
  match lst with
  | [] -> Ok ()
  | x :: xs -> f x >>= fun () -> stop_at_first_error xs ~f

let copy_files ~major ~minor ~src_files =
  let affix = !"_%d%02d" major minor in
  stop_at_first_error src_files ~f:(fun file ->
    let ext_less = Fpath.rem_ext file in
    (* This is completely adhoc. *)
    if not (String.is_suffix ~affix ext_less) then Ok () else begin
      let new_file_path =
        let ext  = Fpath.get_ext file in
        let last = String.length ext_less - String.length affix - 1 in
        (String.with_index_range ~last ext_less) ^ ext
      in
        Log.info (fun m -> m "cp %S %S" file new_file_path);
        (* TODO: have [clean] remove these files. *)
        OS.File.read file
        >>= fun content ->
        (* Produce correct error messages. *)
        let content = !"# 1 %S\n%s" file content in
        OS.File.write new_file_path content
    end
  )

let preprocess ~major ~minor ~src_files =
  let cppo = Conf.tool "cppo" `Build_os in
  stop_at_first_error src_files ~f:(fun file ->
    let ext_less = Fpath.rem_ext file in
    if not (Fpath.has_ext "cppo" ext_less) then Ok () else begin
      let new_file_path =
        let ext = Fpath.get_ext file in
        let filename = Fpath.rem_ext ext_less in
        filename ^ ext
      in
      Log.info (fun m ->
        m "cppo -D 'OCAML_VERSION (%d, %d)' %S -o %S"
          major minor file new_file_path
      );
      (* TODO: have [clean] remove these files. *)
      OS.Cmd.run @@
      Cmd.(cppo % "-D" % !"OCAML_VERSION (%d, %d)" major minor %
          file % "-o" % new_file_path)
    end
  )

let choose_target_version conf =
  let ocaml_conf = Conf.OCaml.v conf `Build_os in
  let (major, minor, _, _) = Conf.OCaml.version ocaml_conf in
  OS.Dir.contents "src"
  >>= fun src_files ->
  copy_files ~major ~minor ~src_files
  >>= fun () ->
  preprocess ~major ~minor ~src_files


let () =
  let build = Pkg.build ~pre:choose_target_version () in
  let opams = [ Pkg.opam_file ~lint_deps_excluding:(Some [ "cppo" ]) "opam" ] in
  Pkg.describe "doc-ock" ~opams ~build @@ fun c ->
  Ok [ Pkg.lib ~exts:Exts.interface "src/docOckTypes";
       Pkg.mllib "src/doc-ock.mllib";
       Pkg.lib ~exts:Exts.interface "test/ocamlary";
       Pkg.test ~run:false ~auto:false ~exts:(Exts.exts [".cmi"; ".cmo"])
         "test/ocamlary";
       Pkg.test ~args:(Cmd.v "_build/test/ocamlary.cmi") "test/testCmi";
       Pkg.test ~args:(Cmd.v "_build/test/ocamlary.cmti") "test/testCmti";
       Pkg.test ~args:(Cmd.v "_build/test/ocamlary.cmt") "test/testCmt"; ]
