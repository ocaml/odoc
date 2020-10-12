open Or_error

let from_odoc ~env ~warn_error input output =
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page _ ->
    Page.load input >>= fun page ->
    let resolve_env = Env.build env (`Page page) in
    let env = Odoc_xref2.Env.set_resolver Odoc_xref2.Env.empty resolve_env in
    let res = List.fold_right (fun child res ->
      res >>= fun _ ->
      match Odoc_xref2.Env.lookup_page child env with
      | Some _ -> Ok ()
      | None ->
      match Odoc_xref2.Env.lookup_root_module child env with
      | Some _ -> Ok ()
      | None -> Error (`Msg (Format.sprintf "Couldn't find child: %s\n%!" child ))) page.Odoc_model.Lang.Page.children (Ok ()) in
    res >>= fun _ ->
    Odoc_xref2.Link.resolve_page env page
    |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error ~filename:input_s
    >>= fun odoctree ->
    Page.save output odoctree;

    Ok ()
  | Compilation_unit {hidden; _} ->
    Compilation_unit.load input >>= fun unit ->
    let unit =
      if hidden
      then {unit with content = Odoc_model.Lang.Compilation_unit.Module []; expansion=None }
      else unit
    in

    let env = Env.build env (`Unit unit) in
    Odoc_xref2.Link.link env unit
    |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error ~filename:input_s
    >>= fun odoctree ->

    Compilation_unit.save output odoctree;

    Ok ()
