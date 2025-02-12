let json ~html_dir ~pkg ?(redirections = Hashtbl.create 0) () =
  let files =
    let lib_dir = Odoc_unit.pkg_dir pkg in
    let lib_dir = Fpath.( // ) html_dir lib_dir in
    let files =
      Bos.OS.Dir.fold_contents
        ~elements:(`Sat (fun x -> Ok (Fpath.has_ext "html" x)))
        (fun path acc ->
          `String
            (Fpath.to_string (Fpath.rem_prefix lib_dir path |> Option.get))
          :: acc)
        [] lib_dir
      |> function
      | Ok e -> e
      | Error (`Msg err) ->
          Logs.err (fun m ->
              m "Got an error while collecting files for status.json: %s" err);
          []
    in
    `List files
  in
  let name = `String pkg.Packages.name in
  let version = `String pkg.Packages.version in
  let failed = `Bool false in
  let redirections =
    Hashtbl.fold
      (fun old_path new_path acc ->
        `Assoc
          [
            ("old_path", `String (Fpath.to_string old_path));
            ("new_path", `String (Fpath.to_string new_path));
          ]
        :: acc)
      redirections []
  in
  let redirections = `List redirections in
  `Assoc
    [
      ("name", name);
      ("version", version);
      ("files", files);
      ("failed", failed);
      ("redirections", redirections);
    ]
