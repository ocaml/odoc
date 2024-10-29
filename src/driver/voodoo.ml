(* Voodoo *)

let ( >>= ) = Result.bind

type pkg = {
  name : string;
  version : string;
  universe : string;
  blessed : bool;
  files : Fpath.t list;
}

let prep_path = ref "prep"

let top_dir pkg =
  if pkg.blessed then Fpath.(v "p" / pkg.name / pkg.version)
  else Fpath.(v "u" / pkg.universe / pkg.name / pkg.version)

(* Use output from Voodoo Prep as input *)

let find_universe_and_version pkg_name =
  Bos.OS.Dir.contents Fpath.(v !prep_path / "universes") >>= fun universes ->
  let universe =
    match
      List.find_opt
        (fun u ->
          match Bos.OS.Dir.exists Fpath.(u / pkg_name) with
          | Ok b -> b
          | Error _ -> false)
        universes
    with
    | Some u -> Ok u
    | None -> Error (`Msg (Format.sprintf "Failed to find package %s" pkg_name))
  in
  universe >>= fun u ->
  Bos.OS.Dir.contents ~rel:true Fpath.(u / pkg_name) >>= fun version ->
  match (Fpath.segs u, version) with
  | _ :: _ :: u :: _, [ version ] -> Ok (u, Fpath.to_string version)
  | _ -> Error (`Msg (Format.sprintf "Failed to find package %s" pkg_name))

(* Given a directory containing for example [a.cma] and [b.cma], this
   function returns a Fpath.Map.t mapping [dir/a.cma -> a] and [dir/b.cma -> b] *)
let libname_of_archives_of_dir dir =
  let files_res = Bos.OS.Dir.contents dir in
  match files_res with
  | Error _ -> Fpath.Map.empty
  | Ok files ->
      List.fold_left
        (fun acc file ->
          let base = Fpath.basename file in
          if Astring.String.is_suffix ~affix:".cma" base then
            let libname = String.sub base 0 (String.length base - 4) in
            Fpath.Map.add Fpath.(dir / libname) libname acc
          else acc)
        Fpath.Map.empty files

let metas_of_pkg pkg =
  List.filter
    (fun p ->
      let filename = Fpath.filename p in
      filename = "META")
    pkg.files

(* Given a [pkg] and an output [pkg_path], returns a pair of lists of assets an mlds *)
let assets_and_mlds_of_pkg pkg_path pkg =
  pkg.files
  |> List.filter_map (fun p ->
         let prefix = Fpath.(v "doc" / pkg.name / "odoc-pages") in
         let asset_prefix = Fpath.(v "doc" / pkg.name / "odoc-assets") in
         let check_name pkg_name =
           if pkg_name <> pkg.name then (
             Logs.err (fun k ->
                 k
                   "Error: name in 'doc' dir does not match package name: %s \
                    <> %s"
                   pkg_name pkg.name);
             None)
           else Some ()
         in
         let ( >>= ) = Option.bind in
         match Fpath.segs p with
         | "doc" :: pkg_name :: "odoc-pages" :: _ :: _ -> (
             check_name pkg_name >>= fun () ->
             match Fpath.rem_prefix prefix p with
             | None -> None
             | Some rel_path ->
                 let path = Fpath.(pkg_path // p) in
                 if Fpath.has_ext "mld" p then
                   Some
                     (`M { Packages.mld_path = path; mld_rel_path = rel_path })
                 else
                   Some
                     (`A
                       { Packages.asset_path = path; asset_rel_path = rel_path })
             )
         | "doc" :: pkg_name :: "odoc-assets" :: _ :: _ -> (
             check_name pkg_name >>= fun () ->
             match Fpath.rem_prefix asset_prefix p with
             | None -> None
             | Some asset_rel_path ->
                 let asset_path = Fpath.(pkg_path // p) in
                 Some (`A { Packages.asset_path; asset_rel_path }))
         | _ -> None)
  |> List.partition_map (function
       | `A asset -> Either.Left asset
       | `M mld -> Either.Right mld)

let process_package pkg =
  let metas = metas_of_pkg pkg in

  let pkg_path =
    Fpath.(v "prep" / "universes" / pkg.universe / pkg.name / pkg.version)
  in

  (* a map from libname to the set of dependencies of that library *)
  let all_lib_deps : Util.StringSet.t Util.StringMap.t =
    List.fold_left
      (fun acc meta ->
        let full_meta_path = Fpath.(pkg_path // meta) in
        let m = Library_names.process_meta_file full_meta_path in
        List.fold_left
          (fun acc lib ->
            Util.StringMap.add lib.Library_names.name
              (Util.StringSet.of_list lib.Library_names.deps)
              acc)
          acc m.libraries)
      Util.StringMap.empty metas
  in

  let assets, mlds = assets_and_mlds_of_pkg pkg_path pkg in

  let config =
    let config_file = Fpath.(v "doc" / pkg.name / "odoc-config.sexp") in
    match Bos.OS.File.read config_file with
    | Error _ -> Global_config.empty
    | Ok s -> Global_config.parse s
  in

  let meta_libraries : Packages.libty list =
    metas
    |> List.filter_map (fun meta_file ->
           let full_meta_path = Fpath.(pkg_path // meta_file) in
           let m = Library_names.process_meta_file full_meta_path in
           let libname_of_archive = Library_names.libname_of_archive m in
           Fpath.Map.iter
             (fun k v -> Logs.debug (fun m -> m "%a,%s\n%!" Fpath.pp k v))
             libname_of_archive;

           let directories = Library_names.directories m in
           Some
             (List.concat_map
                (fun directory ->
                  Logs.debug (fun m ->
                      m "Processing directory: %a\n%!" Fpath.pp directory);
                  Packages.Lib.v ~libname_of_archive ~pkg_name:pkg.name
                    ~dir:directory ~cmtidir:None ~all_lib_deps ~cmi_only_libs:[])
                Fpath.(Set.to_list directories)))
    |> List.flatten
  in

  (* Check the main package lib directory even if there's no meta file *)
  let non_meta_libraries =
    let libdirs_without_meta =
      List.filter
        (fun p ->
          match Fpath.segs p with
          | "lib" :: _ :: _
            when Sys.is_directory Fpath.(pkg_path // p |> to_string) ->
              not
                (List.exists
                   (fun lib ->
                     Fpath.equal
                       Fpath.(to_dir_path lib.Packages.dir)
                       Fpath.(to_dir_path (pkg_path // p)))
                   meta_libraries)
          | _ -> false)
        pkg.files
    in

    Logs.debug (fun m ->
        m "libdirs_without_meta: %a\n%!"
          Fmt.(list ~sep:comma Fpath.pp)
          (List.map (fun p -> Fpath.(pkg_path // p)) libdirs_without_meta));

    Logs.debug (fun m ->
        m "lib dirs: %a\n%!"
          Fmt.(list ~sep:comma Fpath.pp)
          (List.map (fun (lib : Packages.libty) -> lib.dir) meta_libraries));

    List.map
      (fun libdir ->
        let libname_of_archive =
          libname_of_archives_of_dir Fpath.(pkg_path // libdir)
        in
        Logs.debug (fun m ->
            m "Processing directory without META: %a" Fpath.pp libdir);
        Packages.Lib.v ~libname_of_archive ~pkg_name:pkg.name
          ~dir:Fpath.(pkg_path // libdir)
          ~cmtidir:None ~all_lib_deps ~cmi_only_libs:[])
      libdirs_without_meta
    |> List.flatten
  in
  let libraries = meta_libraries @ non_meta_libraries in
  let result =
    {
      Packages.name = pkg.name;
      version = pkg.version;
      libraries;
      mlds;
      assets;
      other_docs = Fpath.Set.empty;
      pkg_dir = top_dir pkg;
      config;
    }
  in
  result

let pp ppf v =
  Format.fprintf ppf "n: %s v: %s u: %s [\n" v.name v.version v.universe;
  List.iter (fun fp -> Format.fprintf ppf "%a\n" Fpath.pp fp) v.files;
  Format.fprintf ppf "]\n%!"

let () = ignore pp

let of_voodoo pkg_name ~blessed =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc -> p :: acc)
      []
      Fpath.(v !prep_path)
  in
  match contents with
  | Error _ -> Util.StringMap.empty
  | Ok c ->
      let sorted = List.sort (fun p1 p2 -> Fpath.compare p1 p2) c in
      let last, packages =
        List.fold_left
          (fun (cur_opt, acc) file ->
            match Fpath.segs file with
            | "prep" :: "universes" :: u :: p :: v :: (_ :: _ as rest)
              when p = pkg_name -> (
                let file = Fpath.v (Astring.String.concat ~sep:"/" rest) in
                match cur_opt with
                | Some cur
                  when cur.name = p && cur.version = v && cur.universe = u ->
                    (Some { cur with files = file :: cur.files }, acc)
                | _ ->
                    ( Some
                        {
                          name = p;
                          version = v;
                          universe = u;
                          blessed;
                          files = [ file ];
                        },
                      cur_opt :: acc ))
            | _ -> (cur_opt, acc))
          (None, []) sorted
      in
      let packages = List.filter_map (fun x -> x) (last :: packages) in
      let packages = List.map process_package packages in
      Util.StringMap.singleton pkg_name (List.hd packages)

let extra_libs_paths compile_dir =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc -> p :: acc)
      [] compile_dir
  in
  match contents with
  | Error _ -> Util.StringMap.empty
  | Ok c ->
      List.fold_left
        (fun acc abs_path ->
          let path = Fpath.rem_prefix compile_dir abs_path |> Option.get in
          match Fpath.segs path with
          | [ "p"; _pkg; _version; "lib"; libname ] ->
              Util.StringMap.add libname path acc
          | [ "u"; _universe; _pkg; _version; "lib"; libname ] ->
              Util.StringMap.add libname path acc
          | _ -> acc)
        Util.StringMap.empty c
