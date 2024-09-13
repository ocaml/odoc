(* Packages *)

type dep = string * Digest.t

(* type id = Odoc.id *)

type intf = { mif_hash : string; mif_path : Fpath.t; mif_deps : dep list }

let pp_intf fmt i = Format.fprintf fmt "intf: %a" Fpath.pp i.mif_path

type src_info = { src_path : Fpath.t }

type impl = {
  mip_path : Fpath.t;
  mip_src_info : src_info option;
  mip_deps : dep list;
}

let pp_impl fmt i = Format.fprintf fmt "impl: %a" Fpath.pp i.mip_path

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
}

type mld = { mld_path : Fpath.t; mld_rel_path : Fpath.t }

let pp_mld fmt m = Format.fprintf fmt "%a" Fpath.pp m.mld_path

type asset = { asset_path : Fpath.t; asset_rel_path : Fpath.t }

let pp_asset fmt m = Format.fprintf fmt "%a" Fpath.pp m.asset_path

type libty = {
  lib_name : string;
  dir : Fpath.t;
  archive_name : string;
  lib_deps : Util.StringSet.t;
  modules : modulety list;
}

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  other_docs : Fpath.Set.t;
  pkg_dir : Fpath.t;
  config : Global_config.t;
}

let maybe_prepend_top top_dir dir =
  match top_dir with None -> dir | Some d -> Fpath.(d // dir)

let pkg_dir top_dir pkg_name = maybe_prepend_top top_dir Fpath.(v pkg_name)

(* let parent_of_lib pkg_dir lib_name = Fpath.(pkg_dir / "lib" / lib_name) *)

let parent_of_pages pkg_dir = Fpath.(pkg_dir / "doc")

(* let parent_of_src pkg_dir lib_name = Fpath.(pkg_dir / "src" / lib_name) *)

module Module = struct
  type t = modulety

  let pp ppf (t : t) =
    Fmt.pf ppf "name: %s@.intf: %a@.impl: %a@.hidden: %b@." t.m_name Fpath.pp
      t.m_intf.mif_path (Fmt.option pp_impl) t.m_impl t.m_hidden

  let is_hidden name = Astring.String.is_infix ~affix:"__" name

  let vs libsdir cmtidir modules =
    let dir = match cmtidir with None -> libsdir | Some dir -> dir in
    let mk m_name =
      let exists ext =
        let p =
          Fpath.(dir // add_ext ext (v (String.uncapitalize_ascii m_name)))
        in
        let upperP =
          Fpath.(dir // add_ext ext (v (String.capitalize_ascii m_name)))
        in
        Logs.debug (fun m ->
            m "Checking %a (then %a)" Fpath.pp p Fpath.pp upperP);
        match Bos.OS.File.exists p with
        | Ok true -> Some p
        | _ -> (
            match Bos.OS.File.exists upperP with
            | Ok true -> Some upperP
            | _ -> None)
      in
      let mk_intf mif_path =
        match Odoc.compile_deps mif_path with
        | Ok { digest; deps } ->
            { mif_hash = digest; mif_path; mif_deps = deps }
        | Error _ -> failwith "bad deps"
      in
      let mk_impl mip_path =
        (* Directories in which we should look for source files *)
        let src_dirs =
          match cmtidir with None -> [ libsdir ] | Some d2 -> [ libsdir; d2 ]
        in

        let mip_src_info =
          match Ocamlobjinfo.get_source mip_path src_dirs with
          | None ->
              Logs.debug (fun m -> m "No source found for module %s" m_name);
              None
          | Some src_path ->
              Logs.debug (fun m ->
                  m "Found source file %a for %s" Fpath.pp src_path m_name);
              Some { src_path }
        in
        let mip_deps =
          match Odoc.compile_deps mip_path with
          | Ok { digest = _; deps } -> deps
          | Error _ -> failwith "bad deps"
        in
        { mip_src_info; mip_path; mip_deps }
      in
      let state = (exists "cmt", exists "cmti") in

      let m_hidden = is_hidden m_name in
      try
        let r (m_intf, m_impl) = Some { m_name; m_intf; m_impl; m_hidden } in
        match state with
        | Some cmt, Some cmti -> r (mk_intf cmti, Some (mk_impl cmt))
        | Some cmt, None -> r (mk_intf cmt, Some (mk_impl cmt))
        | None, Some cmti -> r (mk_intf cmti, None)
        | None, None ->
            Logs.warn (fun m -> m "No files for module: %s" m_name);
            None
      with _ ->
        Logs.err (fun m -> m "Error processing module %s. Ignoring." m_name);
        None
    in

    Eio.Fiber.List.filter_map mk modules
end

module Lib = struct
  let v ~libname_of_archive ~pkg_name ~dir ~cmtidir ~all_lib_deps =
    Logs.debug (fun m ->
        m "Classifying dir %a for package %s" Fpath.pp dir pkg_name);
    let dirs =
      match cmtidir with None -> [ dir ] | Some dir2 -> [ dir; dir2 ]
    in
    let results = Odoc.classify dirs in
    Logs.debug (fun m -> m "Got %d lines" (List.length results));
    List.filter_map
      (fun (archive_name, modules) ->
        try
          let lib_name = Util.StringMap.find archive_name libname_of_archive in
          let modules = Module.vs dir cmtidir modules in
          let lib_deps =
            try Util.StringMap.find lib_name all_lib_deps
            with _ -> Util.StringSet.empty
          in
          Some { lib_name; archive_name; modules; lib_deps; dir }
        with e ->
          Logs.err (fun m ->
              m "Error processing library %s: %s Ignoring." archive_name
                (Printexc.to_string e));
          None)
      results

  let pp ppf t =
    Fmt.pf ppf "archive: %a modules: [@[<hov 2>@,%a@]@,]" Fmt.string
      t.archive_name
      Fmt.(list ~sep:sp Module.pp)
      t.modules
end
let pp ppf t =
  Fmt.pf ppf "name: %s@.version: %s@.libraries: [@[<hov 2>@,%a@]@,]" t.name
    t.version
    Fmt.(list ~sep:sp Lib.pp)
    t.libraries

let of_libs ~packages_dir libs =
  let libs = Util.StringSet.to_seq libs |> List.of_seq in
  let results = List.map (fun x -> (x, Ocamlfind.deps [ x ])) libs in
  let all_libs_set =
    List.fold_left
      (fun acc (lib, r) ->
        match r with
        | Ok x -> Util.StringSet.(union (of_list x) acc)
        | Error (`Msg e) ->
            Logs.err (fun m ->
                m "Error finding dependencies of libraries [%s]: %s" lib e);
            Logs.err (fun m -> m "Will attempt to document the library anyway");
            Util.StringSet.add lib acc)
      Util.StringSet.empty results
  in
  let all_libs = Util.StringSet.elements all_libs_set in
  let all_libs = "stdlib" :: all_libs in
  let all_lib_deps =
    List.fold_right
      (fun lib_name acc ->
        match Ocamlfind.deps [ lib_name ] with
        | Ok deps ->
            Util.StringMap.add lib_name (Util.StringSet.of_list deps) acc
        | Error (`Msg msg) ->
            Logs.err (fun m ->
                m
                  "Error finding dependencies of library '%s' through \
                   ocamlfind: %s"
                  lib_name msg);
            acc)
      all_libs Util.StringMap.empty
  in
  Logs.debug (fun m ->
      m "Libraries to document: [%a]" Fmt.(list ~sep:sp string) all_libs);
  let dirs' =
    List.filter_map
      (fun lib ->
        match Ocamlfind.get_dir lib with
        | Error _ ->
            Logs.debug (fun m -> m "No dir for library %s" lib);
            None
        | Ok p ->
            let archives = Ocamlfind.archives lib in
            Logs.debug (fun m ->
                m "Archives for library %s: [%a]" lib
                  Fmt.(list ~sep:sp string)
                  archives);
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
  let map, rmap =
    (* if Sys.file_exists ".pkg_to_dir_map" then (
         let ic = open_in_bin ".pkg_to_dir_map" in
         let result = Marshal.from_channel ic in
         close_in ic;
         result)
       else *)
    let result = Opam.pkg_to_dir_map () in
    (* let oc = open_out_bin ".pkg_to_dir_map" in
       Marshal.to_channel oc result [];
       close_out oc; *)
    result
  in
  let dirs =
    List.fold_left
      (fun set (_lib, p, archives) ->
        Fpath.Map.update p
          (function
            | Some set -> Some (Util.StringSet.union set archives)
            | None -> Some archives)
          set)
      Fpath.Map.empty dirs'
  in
  let libname_of_archive =
    List.fold_left
      (fun map (lib, _, archives) ->
        match Util.StringSet.elements archives with
        | [] -> map
        | [ archive ] ->
            Util.StringMap.update archive
              (function
                | None -> Some lib
                | Some x ->
                    Logs.err (fun m ->
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
      Util.StringMap.empty dirs'
  in
  ignore libname_of_archive;
  let mk_mlds pkg_name odoc_pages =
    let odig_convention asset_path =
      let asset_prefix =
        Fpath.(v (Opam.prefix ()) / "doc" / pkg_name / "odoc-assets")
      in
      let rel_path = Fpath.rem_prefix asset_prefix asset_path in
      match rel_path with
      | None -> []
      | Some rel_path ->
          [ { asset_path; asset_rel_path = Fpath.(v "_assets" // rel_path) } ]
    in
    let prefix = Fpath.(v (Opam.prefix ()) / "doc" / pkg_name / "odoc-pages") in
    let mlds, assets =
      Fpath.Set.fold
        (fun path (mld_acc, asset_acc) ->
          let rel_path = Fpath.rem_prefix prefix path in
          match rel_path with
          | None -> (mld_acc, odig_convention path @ asset_acc)
          | Some rel_path ->
              if Fpath.has_ext "mld" path then
                ( { mld_path = path; mld_rel_path = rel_path } :: mld_acc,
                  asset_acc )
              else
                ( mld_acc,
                  { asset_path = path; asset_rel_path = rel_path } :: asset_acc
                ))
        odoc_pages ([], [])
    in
    (mlds, assets)
  in
  let global_config (pkg_name : string) =
    let config_file =
      Fpath.(v (Opam.prefix ()) / "doc" / pkg_name / "odoc-config.sexp")
    in
    match Bos.OS.File.read config_file with
    | Error _ -> Global_config.empty
    | Ok s -> Global_config.parse s
  in
  Fpath.Map.fold
    (fun dir archives acc ->
      match Fpath.Map.find dir rmap with
      | None ->
          Logs.debug (fun m -> m "No package for dir %a\n%!" Fpath.pp dir);
          acc
      | Some pkg ->
          let libraries =
            Lib.v ~libname_of_archive ~pkg_name:pkg.name ~dir ~cmtidir:None
              ~all_lib_deps
          in
          let libraries =
            List.filter
              (fun l -> Util.StringSet.mem l.archive_name archives)
              libraries
          in
          Util.StringMap.update pkg.name
            (function
              | Some pkg ->
                  let libraries = libraries @ pkg.libraries in
                  Some { pkg with libraries }
              | None ->
                  let pkg_dir = pkg_dir packages_dir pkg.name in
                  let config = global_config pkg.name in
                  let pkg', { Opam.odoc_pages; other_docs; _ } =
                    List.find
                      (fun (pkg', _) ->
                        (* Logs.debug (fun m ->
                            m "Checking %s against %s" pkg.Opam.name pkg'.Opam.name); *)
                        pkg = pkg')
                      map
                  in
                  let mlds, assets = mk_mlds pkg'.name odoc_pages in
                  Logs.debug (fun m ->
                      m "%d mlds for package %s (from %d odoc_pages)"
                        (List.length mlds) pkg.name
                        (Fpath.Set.cardinal odoc_pages));
                  Some
                    {
                      name = pkg.name;
                      version = pkg.version;
                      libraries;
                      mlds;
                      assets;
                      other_docs;
                      pkg_dir;
                      config;
                    })
            acc)
    dirs Util.StringMap.empty

type set = t Util.StringMap.t
