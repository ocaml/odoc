open Or_error

let from_odoc ~env input =
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
    Page.save Fs.File.(set_ext ".odocl" input) odoctree;

    Ok ()
  | Compilation_unit {hidden; _} ->
    Compilation_unit.load input >>= fun unit ->
    let unit =
      if hidden
      then {unit with content = Odoc_model.Lang.Compilation_unit.Module []; expansion=None }
      else unit
    in

    let odoctree =
      let env = Env.build env (`Unit unit) in
      Odoc_xref2.Link.link env unit
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in

    Compilation_unit.save Fs.File.(set_ext ".odocl" input) odoctree;

    Ok ()
