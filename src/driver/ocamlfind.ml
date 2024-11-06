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
    Ok Fpath.(v x.package_dir |> to_dir_path)
  with e ->
    Logs.err (fun m -> m "Error: %s\n" (Printexc.to_string e));
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

(* Returns deep dependencies for the given package *)
let rec dep =
  let memo = ref Util.StringMap.empty in
  fun pkg ->
    init ();
    try Util.StringMap.find pkg !memo
    with Not_found -> (
      try
        let deps = Fl_package_base.requires ~preds:[ "ppx_driver" ] pkg in
        let result =
          List.fold_left
            (fun acc x ->
              match dep x with
              | Ok dep_deps -> Util.StringSet.(union acc (add x dep_deps))
              | Error _ -> acc)
            Util.StringSet.empty deps
        in
        memo := Util.StringMap.add pkg (Ok result) !memo;
        Ok result
      with e ->
        let result = Error (`Msg (Printexc.to_string e)) in
        memo := Util.StringMap.add pkg result !memo;
        result)

let deps pkgs =
  let results = List.map dep pkgs in
  Ok
    (List.fold_left Util.StringSet.union
       (Util.StringSet.singleton "stdlib")
       (List.map (Result.value ~default:Util.StringSet.empty) results))

module Db = struct
  type t = {
    all_libs : Util.StringSet.t;
    all_lib_deps : Util.StringSet.t Util.StringMap.t;
    lib_dirs_and_archives : (string * Fpath.t * Util.StringSet.t) list;
    archives_by_dir : Util.StringSet.t Fpath.map;
    libname_of_archive : string Fpath.map;
    cmi_only_libs : (Fpath.t * string) list;
  }

  let create libs =
    let _ = Opam.prefix () in
    let libs = Util.StringSet.to_seq libs |> List.of_seq in

    (* First, find the complete set of libraries - that is, including all of
       the dependencies of the libraries supplied on the commandline *)
    let all_libs_deps =
      match deps libs with
      | Error (`Msg msg) ->
          Logs.err (fun m -> m "Error finding dependencies: %s" msg);
          Util.StringSet.empty
      | Ok libs -> Util.StringSet.add "stdlib" libs
    in

    let all_libs_set =
      Util.StringSet.union all_libs_deps (Util.StringSet.of_list libs)
    in
    let all_libs = Util.StringSet.elements all_libs_set in

    (* Now we need the dependency tree of those libraries *)
    let all_lib_deps =
      List.fold_right
        (fun lib_name acc ->
          match deps [ lib_name ] with
          | Ok deps -> Util.StringMap.add lib_name deps acc
          | Error (`Msg msg) ->
              Logs.err (fun m ->
                  m
                    "Error finding dependencies of library '%s' through \
                     ocamlfind: %s"
                    lib_name msg);
              acc)
        all_libs Util.StringMap.empty
    in

    (* We also need to find, for each library, the library directory and
       the list of archives for that library *)
    let lib_dirs_and_archives =
      List.filter_map
        (fun lib ->
          match get_dir lib with
          | Error _ ->
              Logs.err (fun m -> m "No dir for library %s" lib);
              None
          | Ok p ->
              let archives = archives lib in
              let archives =
                List.map
                  (fun x ->
                    try Filename.chop_extension x
                    with e ->
                      Logs.err (fun m -> m "Can't chop extension from %s" x);
                      raise e)
                  archives
              in
              let archives = Util.StringSet.(of_list archives) in
              Some (lib, p, archives))
        all_libs
    in

    (* An individual directory may contain multiple libraries, each with
       zero or more archives. We need to know which directories contain
       which archives *)
    let archives_by_dir =
      List.fold_left
        (fun set (_lib, p, archives) ->
          Fpath.Map.update p
            (function
              | Some set -> Some (Util.StringSet.union set archives)
              | None -> Some archives)
            set)
        Fpath.Map.empty lib_dirs_and_archives
    in

    (* Compute the mapping between full path of an archive to the
       name of the libary *)
    let libname_of_archive =
      List.fold_left
        (fun map (lib, dir, archives) ->
          match Util.StringSet.elements archives with
          | [] -> map
          | [ archive ] ->
              Fpath.Map.update
                Fpath.(dir / archive)
                (function
                  | None -> Some lib
                  | Some x ->
                      Logs.info (fun m ->
                          m
                            "Multiple libraries for archive %s: %s and %s. \
                             Arbitrarily picking the latter."
                            archive x lib);
                      Some lib)
                map
          | xs ->
              Logs.err (fun m ->
                  m "multiple archives detected: [%a]"
                    Fmt.(list ~sep:sp string)
                    xs);
              assert false)
        Fpath.Map.empty lib_dirs_and_archives
    in

    (* We also need to know about libraries that have no archives at all
       (these are virtual libraries usually) *)
    let cmi_only_libs =
      List.fold_left
        (fun map (lib, dir, archives) ->
          match Util.StringSet.elements archives with
          | [] -> (dir, lib) :: map
          | _ -> map)
        [] lib_dirs_and_archives
    in
    {
      all_libs = all_libs_set;
      all_lib_deps;
      lib_dirs_and_archives;
      archives_by_dir;
      libname_of_archive;
      cmi_only_libs;
    }
end
