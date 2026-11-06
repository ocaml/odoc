(* Voodoo *)

type pkg = {
  name : string;
  version : string;
  universe : string;
  blessed : bool;
  files : Fpath.t list;
}

(* Where voodoo-prep put the packages' files. *)
let prep = Fpath.v "prep"

let top_dir pkg =
  if pkg.blessed then Fpath.(v "p" / pkg.name / pkg.version)
  else Fpath.(v "u" / pkg.universe / pkg.name / pkg.version)

(* Use output from Voodoo Prep as input *)

let of_voodoo pkg =
  let pkg_path =
    Fpath.(prep / "universes" / pkg.universe / pkg.name / pkg.version)
  in
  (* The package's libraries are read by findlib, as every other library is.
     Its [lib] is put first on findlib's path, so a query by name finds this
     copy of them. *)
  (let lib = Fpath.(pkg_path / "lib") in
   if Bos.OS.Dir.exists lib = Ok true then Ocamlfind.add_path lib);
  let libs =
    List.concat_map
      (fun p ->
        if Fpath.filename p = "META" then
          Ocamlfind.libraries_of_meta Fpath.(pkg_path // p)
        else [])
      pkg.files
  in
  let installed = Installed.of_prep ~pkg_path ~pkg_name:pkg.name ~libs in

  let docs = Opam.classify_docs pkg_path (Some pkg.name) pkg.files in
  let mlds, assets, other_docs = Packages.mk_mlds docs in

  let config =
    let config_file =
      Fpath.(pkg_path / "doc" / pkg.name / "odoc-config.sexp")
    in
    match Bos.OS.File.read config_file with
    | Error (`Msg msg) ->
        Logs.debug (fun m ->
            m "No config file found: %a\n%s\n%!" Fpath.pp config_file msg);
        Global_config.empty
    | Ok s ->
        Logs.debug (fun m -> m "Config file: %a\n%!" Fpath.pp config_file);
        Global_config.parse s
  in

  Logs.debug (fun m ->
      m "Config.packages: %s\n%!" (String.concat ", " config.deps.packages));
  (* The directories the libraries are in. A META may name a directory that is
     not installed. topkg points at a ../topkg-care directory that belongs to
     another package; and a package built without an optional dependency
     still declares the sub-library it did not build, as fmt declares fmt.cli
     when cmdliner was absent. Either way there is nothing to document, but
     say so: a library silently missing from the output is hard to account
     for later. *)
  let directories =
    List.fold_left
      (fun acc lib ->
        match Installed.find installed lib with
        | Some { dir; _ } when Bos.OS.Dir.exists dir = Ok true ->
            Fpath.Set.add dir acc
        | Some { dir; _ } ->
            Logs.info (fun m ->
                m
                  "Library %s says its files are in %a, which does not exist, \
                   so it will not be documented"
                  lib Fpath.pp dir);
            acc
        | None -> acc)
      Fpath.Set.empty libs
  in
  let meta_libraries : Packages.libty list =
    List.concat_map
      (fun dir ->
        Logs.debug (fun m -> m "Processing directory: %a" Fpath.pp dir);
        Packages.Lib.v ~roots:[ pkg_path ] ~installed ~pkg_name:pkg.name ~dir)
      (Fpath.Set.to_list directories)
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
        (* No library declares these archives, so each is documented as a
           library named after it. *)
        Logs.debug (fun m ->
            m "Processing directory without META: %a" Fpath.pp libdir);
        Packages.Lib.v ~roots:[ pkg_path ] ~installed ~pkg_name:pkg.name
          ~dir:Fpath.(pkg_path // libdir))
      libdirs_without_meta
    |> List.flatten
  in
  let libraries = meta_libraries @ non_meta_libraries in
  let pkg_dir = top_dir pkg in
  let doc_dir = Fpath.(pkg_dir / "doc") in
  let result =
    {
      Packages.name = pkg.name;
      version = pkg.version;
      libraries;
      mlds;
      assets;
      other_docs;
      pkg_dir;
      doc_dir;
      config;
      doc_deps = [];
    }
  in
  (result, installed)

let find_pkg pkg_name ~blessed =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true (fun p acc -> p :: acc) [] prep
  in
  match contents with
  | Error _ -> None
  | Ok c -> (
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
      match packages with
      | [ package ] -> Some package
      | [] ->
          Logs.err (fun m -> m "No package found for %s" pkg_name);
          None
      | _ ->
          Logs.err (fun m -> m "Multiple packages found for %s" pkg_name);
          None)

let occurrence_file_of_pkg pkg =
  let top_dir = top_dir pkg in
  Fpath.(top_dir / "occurrences-all.odoc-occurrences")
