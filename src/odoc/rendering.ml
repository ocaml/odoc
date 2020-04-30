open Odoc_document
open Or_error

let render_document renderer ~output:root_dir ~extra odoctree =
  let pages = renderer.Renderer.render extra odoctree in
  Renderer.traverse pages ~f:(fun filename content ->
    let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
    let directory = Fs.File.dirname filename in
    Fs.Directory.mkdir_p directory;
    let oc = open_out (Fs.File.to_string filename) in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%t@?" content;
    close_out oc
  );
  Ok ()

let document_of_input ~env ~syntax input =
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page _ ->
    Page.load input >>= fun page ->
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Odoc_xref2.Link.resolve_page resolve_env page
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    Ok (Renderer.document_of_page ~syntax odoctree)
  | Compilation_unit {hidden; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    Compilation_unit.load input >>= fun unit ->
    let unit =
      if hidden
      then {unit with content = Odoc_model.Lang.Compilation_unit.Module []; expansion=None }
      else unit
    in
    let odoctree =
      let env = Env.build env (`Unit unit) in
      (* let startlink = Unix.gettimeofday () in *)
      (* Format.fprintf Format.err_formatter "**** Link...\n%!"; *)
      let linked = Odoc_xref2.Link.link env unit in
      (* let finishlink = Unix.gettimeofday () in *)
      (* Format.fprintf Format.err_formatter "**** Finished: Link=%f\n%!" (finishlink -. startlink); *)
      (* Printf.fprintf stderr "num_times: %d\n%!" !Odoc_xref2.Tools.num_times; *)
      linked
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in

    Odoc_xref2.Tools.reset_caches ();
    Hashtbl.clear Compilation_unit.units_cache;

    Compilation_unit.save Fs.File.(set_ext ".odocl" input) odoctree;
    Ok (Renderer.document_of_compilation_unit ~syntax odoctree)

let from_odoc ~env ~syntax ~renderer ~output extra file =
  document_of_input ~env ~syntax file
  >>= render_document renderer ~output ~extra
  
