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

let process_package pkg =
  let metas =
    List.filter
      (fun p ->
        let filename = Fpath.filename p in
        filename = "META")
      pkg.files
  in

  let pkg_path =
    Fpath.(v "prep" / "universes" / pkg.universe / pkg.name / pkg.version)
  in
  let assets, mlds =
    List.filter_map
      (fun p ->
        let prefix = Fpath.(v "doc" / pkg.name / "odoc-pages") in
        let asset_prefix = Fpath.(v "doc" / pkg.name / "odoc-assets") in
        let check_name pkg_name =
          if pkg_name <> pkg.name then (
            Logs.err (fun k ->
                k
                  "Error: name in 'doc' dir does not match package name: %s <> \
                   %s"
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
      pkg.files
    |> List.partition_map (function
         | `A asset -> Either.Left asset
         | `M mld -> Either.Right mld)
  in
  let config =
    let config_file = Fpath.(v "doc" / pkg.name / "odoc-config.sexp") in
    match Bos.OS.File.read config_file with
    | Error _ -> Global_config.empty
    | Ok s -> Global_config.parse s
  in
  let libraries =
    List.filter_map
      (fun meta_file ->
        let full_meta_path = Fpath.(pkg_path // meta_file) in
        let libs = Library_names.process_meta_file full_meta_path in
        let meta_dir = Fpath.parent full_meta_path in
        let directories =
          List.fold_left
            (fun acc x ->
              match x.Library_names.dir with
              | None -> Fpath.Set.add meta_dir acc
              | Some x -> (
                  let dir = Fpath.(meta_dir // v x) in
                  (* NB. topkg installs a META file that points to a ../topkg-care directory
                     that is installed by the topkg-care package. We filter that out here,
                     though I've not thought of a good way to sort out the `topkg-care` package *)
                  match Bos.OS.Dir.exists dir with
                  | Ok true -> Fpath.Set.add dir acc
                  | _ -> acc))
            Fpath.Set.empty libs
        in
        let libname_of_archive =
          List.fold_left
            (fun acc x ->
              Util.StringMap.update x.Library_names.archive_name
                (function
                  | None -> Some x.Library_names.name
                  | Some y ->
                      Logs.err (fun m ->
                          m "Multiple libraries for archive %s: %s and %s."
                            x.archive_name x.name y);
                      Some y)
                acc)
            Util.StringMap.empty libs
        in
        Util.StringMap.iter
          (fun k v -> Logs.debug (fun m -> m "%s,%s\n%!" k v))
          libname_of_archive;
        Some
          (List.concat_map
             (fun directory ->
               Format.eprintf "Processing directory: %a\n%!" Fpath.pp directory;
               Packages.Lib.v ~libname_of_archive ~pkg_name:pkg.name
                 ~dir:directory ~cmtidir:None)
             Fpath.(Set.to_list directories)))
      metas
  in
  (* Check the main package lib directory even if there's no meta file *)
  let extra_libraries =
    let libdirs_without_meta =
      List.filter
        (fun p ->
          match Fpath.segs p with
          | "lib" :: _ :: _
            when Sys.is_directory Fpath.(pkg_path // p |> to_string) ->
              not
                (List.exists
                   (fun p2 -> Fpath.parent p2 = Fpath.to_dir_path p)
                   metas)
          | _ -> false)
        pkg.files
    in
    List.map
      (fun libdir ->
        Logs.debug (fun m ->
            m "Processing directory without META: %a" Fpath.pp libdir);
        Packages.Lib.v ~libname_of_archive:Util.StringMap.empty
          ~pkg_name:pkg.name
          ~dir:Fpath.(pkg_path // libdir)
          ~cmtidir:None)
      libdirs_without_meta
  in
  Printf.eprintf "Found %d metas" (List.length metas);
  let libraries = List.flatten libraries in
  let libraries = List.flatten extra_libraries @ libraries in
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
