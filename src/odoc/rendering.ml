open Odoc_document
open Or_error

let document_of_odocl ~syntax input =
  Compilation_unit.load input >>= fun unit ->
  match unit.content with
  | Compilation_unit.Page_content odoctree ->
      Ok (Renderer.document_of_page ~syntax odoctree)
  | Module_content odoctree ->
      Ok (Renderer.document_of_compilation_unit ~syntax odoctree)

let document_of_input ~env ~warn_error ~syntax input =
  let input_s = Fs.File.to_string input in
  Compilation_unit.load input >>= fun unit ->
  match unit.content with
  | Compilation_unit.Page_content page ->
      let resolve_env = Env.build_from_page env page in
      Odoc_xref2.Link.resolve_page resolve_env page
      |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error
           ~filename:input_s
      >>= fun odoctree -> Ok (Renderer.document_of_page ~syntax odoctree)
  | Module_content m ->
      (* If hidden, we should not generate HTML. See
           https://github.com/ocaml/odoc/issues/99. *)
      let m =
        if Odoc_model.Root.Odoc_file.hidden m.root.file then
          {
            m with
            content =
              Odoc_model.Lang.Compilation_unit.Module
                { items = []; compiled = false; doc = [] };
            expansion = None;
          }
        else m
      in
      let env = Env.build_from_module env m in
      (* let startlink = Unix.gettimeofday () in *)
      (* Format.fprintf Format.err_formatter "**** Link...\n%!"; *)
      let linked = Odoc_xref2.Link.link env m in
      (* let finishlink = Unix.gettimeofday () in *)
      (* Format.fprintf Format.err_formatter "**** Finished: Link=%f\n%!" (finishlink -. startlink); *)
      (* Printf.fprintf stderr "num_times: %d\n%!" !Odoc_xref2.Tools.num_times; *)
      linked
      |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error
           ~filename:input_s
      >>= fun odoctree ->
      Odoc_xref2.Tools.reset_caches ();

      Compilation_unit.save_module Fs.File.(set_ext ".odocl" input) odoctree;
      Ok (Renderer.document_of_compilation_unit ~syntax odoctree)

let render_document renderer ~output:root_dir ~extra odoctree =
  let pages = renderer.Renderer.render extra odoctree in
  Renderer.traverse pages ~f:(fun filename content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc);
  Ok ()

let render_odoc ~env ~warn_error ~syntax ~renderer ~output extra file =
  document_of_input ~env ~warn_error ~syntax file
  >>= render_document renderer ~output ~extra

let generate_odoc ~syntax ~renderer ~output extra file =
  document_of_odocl ~syntax file >>= render_document renderer ~output ~extra

let targets_odoc ~env ~warn_error ~syntax ~renderer ~output:root_dir ~extra
    odoctree =
  let doc =
    if Fpath.get_ext odoctree = ".odoc" then
      document_of_input ~env ~warn_error ~syntax odoctree
    else document_of_odocl ~syntax:OCaml odoctree
  in
  doc >>= fun odoctree ->
  let pages = renderer.Renderer.render extra odoctree in
  Renderer.traverse pages ~f:(fun filename _content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      Format.printf "%a\n" Fpath.pp filename);
  Ok ()
