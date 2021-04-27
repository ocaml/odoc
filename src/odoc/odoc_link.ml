open Or_error

let from_odoc ~resolver ~warn_error input output =
  let input_s = Fs.File.to_string input in
  Odoc_file.load input >>= function
  | Page_content page ->
      let env = Resolver.build_env_for_page resolver page in
      Odoc_xref2.Link.resolve_page ~filename:input_s env page
      |> Odoc_model.Error.handle_warnings ~warn_error
      >>= fun odoctree ->
      Odoc_file.save_page output odoctree;

      Ok ()
  | Unit_content m ->
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

      let env = Resolver.build_env_for_unit resolver m in
      Odoc_xref2.Link.link ~filename:input_s env m
      |> Odoc_model.Error.handle_warnings ~warn_error:false
      >>= fun odoctree ->
      Odoc_file.save_unit output odoctree;

      Ok ()
