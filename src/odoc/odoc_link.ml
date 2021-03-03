open Or_error

let from_odoc ~env ~warn_error input output =
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page _ ->
      Page.load input >>= fun page ->
      let resolve_env = Env.build env (`Page page) in
      Odoc_xref2.Link.resolve_page resolve_env page
      |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error
           ~filename:input_s
      >>= fun odoctree ->
      Page.save output odoctree;

      Ok ()
  | Compilation_unit { hidden; _ } ->
      Compilation_unit.load input >>= fun unit ->
      let unit =
        if hidden then
          {
            unit with
            content =
              Odoc_model.Lang.Compilation_unit.Module
                { items = []; compiled = false; doc = [] };
            expansion = None;
          }
        else unit
      in

      let env = Env.build env (`Unit unit) in
      Odoc_xref2.Link.link env unit
      |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error:false
           ~filename:input_s
      >>= fun odoctree ->
      Compilation_unit.save output odoctree;

      Ok ()
