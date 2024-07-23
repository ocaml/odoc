let init =
  let initialized = ref false in
  fun () ->
    if !initialized then ()
    else
      let prefix = Opam.prefix () in
      let env_camllib = Fpath.(v prefix / "lib" / "ocaml" |> to_string) in
      let config = Fpath.(v prefix / "lib" / "findlib.conf" |> to_string) in
      Findlib.init ~config ~env_camllib ()

let all () =
  init ();
  Fl_package_base.list_packages ()

let get_dir lib =
  try
    init ();
    Fl_package_base.query lib |> fun x ->
    Logs.debug (fun m -> m "Package %s is in directory %s@." lib x.package_dir);
    Ok Fpath.(v x.package_dir |> to_dir_path)
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    Error (`Msg "Error getting directory")

let archives pkg =
  init ();
  let package = Fl_package_base.query pkg in
  let get_1 preds =
    try
      [
        Fl_metascanner.lookup "archive" preds
          package.Fl_package_base.package_defs;
      ]
    with _ -> []
  in
  match pkg with
  | "stdlib" -> [ "stdlib.cma"; "stdlib.cmxa" ]
  | _ ->
      get_1 [ "native" ] @ get_1 [ "byte" ]
      @ get_1 [ "native"; "ppx_driver" ]
      @ get_1 [ "byte"; "ppx_driver" ]
      |> List.filter (fun x -> String.length x > 0)
      |> List.sort_uniq String.compare

let sub_libraries top =
  init ();
  let packages = Fl_package_base.list_packages () in
  List.fold_left
    (fun acc lib ->
      let package = String.split_on_char '.' lib |> List.hd in
      if package = top then Util.StringSet.add lib acc else acc)
    Util.StringSet.empty packages

let deps pkgs =
  init ();
  try
    let packages =
      Fl_package_base.requires_deeply ~preds:[ "ppx_driver" ] pkgs
    in
    Ok packages
  with e -> Error (`Msg (Printexc.to_string e))
