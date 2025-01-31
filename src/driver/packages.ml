(* Packages *)

type dep = string * Digest.t

(* type id = Odoc.id *)

type intf = { mif_hash : string; mif_path : Fpath.t; mif_deps : dep list }

let pp_intf fmt (i : intf) =
  Format.fprintf fmt "@[<hov>{@,mif_hash: %s;@,mif_path: %a;@,mif_deps: %a@,}@]"
    i.mif_hash Fpath.pp i.mif_path
    (Fmt.Dump.list (Fmt.Dump.pair Fmt.string Fmt.string))
    i.mif_deps

type src_info = { src_path : Fpath.t }

let pp_src_info fmt i =
  Format.fprintf fmt "@[<hov>{@,src_path: %a@,}@]" Fpath.pp i.src_path

type impl = {
  mip_path : Fpath.t;
  mip_src_info : src_info option;
  mip_deps : dep list;
}

let pp_impl fmt i =
  Format.fprintf fmt
    "@[<hov>{@,mip_path: %a;@,mip_src_info: %a;@,mip_deps: %a@,}@]" Fpath.pp
    i.mip_path
    (Fmt.Dump.option pp_src_info)
    i.mip_src_info
    (Fmt.Dump.list (Fmt.Dump.pair Fmt.string Fmt.string))
    i.mip_deps

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
}

let pp_modulety fmt i =
  Format.fprintf fmt
    "@[<hov>{@,m_name: %s;@,m_intf: %a;@,m_impl: %a;@,m_hidden: %b@,}@]"
    i.m_name pp_intf i.m_intf (Fmt.Dump.option pp_impl) i.m_impl i.m_hidden

type mld = { mld_path : Fpath.t; mld_rel_path : Fpath.t }

type md = { md_path : Fpath.t; md_rel_path : Fpath.t }

let pp_mld fmt m =
  Format.fprintf fmt "@[<hov>{@,mld_path: %a;@,mld_rel_path: %a@,}@]" Fpath.pp
    m.mld_path Fpath.pp m.mld_rel_path

let pp_md fmt m =
  Format.fprintf fmt "@[<hov>{@,md_path: %a;@,md_rel_path: %a@,}@]" Fpath.pp
    m.md_path Fpath.pp m.md_rel_path

type asset = { asset_path : Fpath.t; asset_rel_path : Fpath.t }

let pp_asset fmt m =
  Format.fprintf fmt "@[<hov>{@,asset_path: %a;@,asset_rel_path: %a@,}@]"
    Fpath.pp m.asset_path Fpath.pp m.asset_rel_path

type libty = {
  lib_name : string;
  dir : Fpath.t;
  archive_name : string option;
  lib_deps : Util.StringSet.t;
  modules : modulety list;
  id_override : string option;
}

let pp_libty fmt l =
  Format.fprintf fmt
    "@[<hov>{@,\
     lib_name: %s;@,\
     dir: %a;@,\
     archive_name: %a;@,\
     lib_deps: %a;@,\
     modules: %a@,\
     id_override: %a@,\n\
    \     }@]"
    l.lib_name Fpath.pp l.dir
    (Fmt.Dump.option Fmt.string)
    l.archive_name
    (Fmt.list ~sep:Fmt.comma Fmt.string)
    (Util.StringSet.elements l.lib_deps)
    (Fmt.Dump.list pp_modulety)
    l.modules
    Fmt.Dump.(option string)
    l.id_override

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  selected : bool;
  remaps : (string * string) list;
  other_docs : md list;
  pkg_dir : Fpath.t;
  doc_dir : Fpath.t;
  config : Global_config.t;
}

let pp fmt t =
  Format.fprintf fmt
    "@[<hov>{@,\
     name: %s;@,\
     version: %s;@,\
     libraries: %a;@,\
     mlds: %a;@,\
     assets: %a;@,\
     selected: %b;@,\
     other_docs: %a;@,\
     pkg_dir: %a@,\
     }@]"
    t.name t.version (Fmt.Dump.list pp_libty) t.libraries (Fmt.Dump.list pp_mld)
    t.mlds (Fmt.Dump.list pp_asset) t.assets t.selected (Fmt.Dump.list pp_md)
    t.other_docs Fpath.pp t.pkg_dir

let maybe_prepend_top top_dir dir =
  match top_dir with None -> dir | Some d -> Fpath.(d // dir)

let pkg_dir top_dir pkg_name = maybe_prepend_top top_dir Fpath.(v pkg_name)

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
            Logs.info (fun m -> m "No files for module: %s" m_name);
            None
      with _ ->
        Logs.err (fun m -> m "Error processing module %s. Ignoring." m_name);
        None
    in

    Eio.Fiber.List.filter_map mk modules
end

module Lib = struct
  let handle_virtual_lib ~dir ~id_override ~lib_name ~all_lib_deps =
    let modules =
      match
        Bos.OS.Dir.fold_contents
          (fun p acc ->
            if Fpath.has_ext "cmti" p then
              let m_name = Fpath.rem_ext p |> Fpath.basename in
              m_name :: acc
            else acc)
          [] dir
      with
      | Ok x -> x
      | Error (`Msg e) ->
          Logs.err (fun m -> m "Error reading dir %a: %s" Fpath.pp dir e);
          []
    in
    let modules = Module.vs dir None modules in
    let lib_deps =
      try Util.StringMap.find lib_name all_lib_deps
      with _ -> Util.StringSet.empty
    in
    [ { lib_name; archive_name = None; modules; lib_deps; dir; id_override } ]

  let v ~libname_of_archive ~pkg_name ~dir ~cmtidir ~all_lib_deps ~cmi_only_libs
      ~id_override =
    Logs.debug (fun m ->
        m "Classifying dir %a for package %s" Fpath.pp dir pkg_name);
    let dirs =
      match cmtidir with None -> [ dir ] | Some dir2 -> [ dir; dir2 ]
    in
    let results = Odoc.classify dirs in
    match List.length results with
    | 0 -> (
        match
          List.find_opt (fun dir -> List.mem_assoc dir cmi_only_libs) dirs
        with
        | None -> []
        | Some dir ->
            let lib_name = List.assoc dir cmi_only_libs in
            handle_virtual_lib ~dir ~lib_name ~all_lib_deps ~id_override)
    | _ ->
        Logs.debug (fun m -> m "Got %d lines" (List.length results));
        List.filter_map
          (fun (archive_name, modules) ->
            match
              Fpath.Map.find Fpath.(dir / archive_name) libname_of_archive
            with
            | Some lib_name ->
                let modules = Module.vs dir cmtidir modules in
                let lib_deps =
                  try Util.StringMap.find lib_name all_lib_deps
                  with _ -> Util.StringSet.empty
                in
                Some
                  {
                    lib_name;
                    archive_name = Some archive_name;
                    modules;
                    lib_deps;
                    dir;
                    id_override;
                  }
            | None ->
                Logs.info (fun m ->
                    m "No entry for '%a' in libname_of_archive" Fpath.pp
                      Fpath.(dir / archive_name));
                Logs.info (fun m ->
                    m "Unable to determine library of archive %s: Ignoring."
                      archive_name);
                None)
          results

  let pp ppf t =
    Fmt.pf ppf "archive: %a modules: [@[<hov 2>@,%a@]@,]"
      Fmt.(option string)
      t.archive_name
      Fmt.(list ~sep:sp Module.pp)
      t.modules
end

(* Construct the list of mlds and assets from a package name and its list of pages *)
let mk_mlds docs =
  List.fold_left
    (fun (mlds, assets, others) (doc : Opam.doc_file) ->
      match doc.kind with
      | `Mld ->
          ( { mld_path = doc.file; mld_rel_path = doc.rel_path } :: mlds,
            assets,
            others )
      | `Asset ->
          ( mlds,
            { asset_path = doc.file; asset_rel_path = doc.rel_path } :: assets,
            others )
      | `Other ->
          ( mlds,
            assets,
            { md_path = doc.file; md_rel_path = doc.rel_path } :: others ))
    ([], [], []) docs

let fix_missing_deps pkgs =
  let lib_name_by_hash =
    List.fold_right
      (fun pkg acc ->
        List.fold_left
          (fun acc lib ->
            List.fold_left
              (fun acc m ->
                Util.StringMap.update m.m_intf.mif_hash
                  (function
                    | None -> Some [ lib.lib_name ]
                    | Some l -> Some (lib.lib_name :: l))
                  acc)
              acc lib.modules)
          acc pkg.libraries)
      pkgs Util.StringMap.empty
  in
  List.map
    (fun pkg ->
      let libraries =
        List.map
          (fun lib ->
            let lib_deps = lib.lib_deps in
            let new_lib_deps =
              List.fold_left
                (fun acc m ->
                  let if_deps =
                    Util.StringSet.of_list (List.map snd m.m_intf.mif_deps)
                  in
                  let impl_deps =
                    match m.m_impl with
                    | Some i -> Util.StringSet.of_list (List.map snd i.mip_deps)
                    | None -> Util.StringSet.empty
                  in
                  let deps = Util.StringSet.union if_deps impl_deps in
                  Util.StringSet.fold
                    (fun hash acc ->
                      match Util.StringMap.find hash lib_name_by_hash with
                      | exception Not_found -> acc
                      | deps ->
                          if
                            List.mem lib.lib_name deps
                            || List.exists
                                 (fun d -> Util.StringSet.mem d lib_deps)
                                 deps
                          then acc
                          else Util.StringSet.add (List.hd deps) acc)
                    deps acc)
                Util.StringSet.empty lib.modules
            in
            if Util.StringSet.cardinal new_lib_deps > 0 then
              Logs.debug (fun m ->
                  m "Adding missing deps to %s: %a" lib.lib_name
                    Fmt.(list string)
                    (Util.StringSet.elements new_lib_deps));
            { lib with lib_deps = Util.StringSet.union new_lib_deps lib_deps })
          pkg.libraries
      in
      { pkg with libraries })
    pkgs

let of_libs ~packages_dir libs =
  let Ocamlfind.Db.
        { archives_by_dir; libname_of_archive; cmi_only_libs; all_lib_deps; _ }
      =
    Ocamlfind.Db.create libs
  in

  (* Opam gives us a map of packages to directories, and vice-versa *)
  let opam_map, opam_rmap = Opam.pkg_to_dir_map () in

  (* Now we can construct the packages *)
  let packages =
    Fpath.Map.fold
      (fun dir archives acc ->
        match Fpath.Map.find dir opam_rmap with
        | None ->
            Logs.debug (fun m -> m "No package for dir %a\n%!" Fpath.pp dir);
            acc
        | Some pkg ->
            let libraries =
              Lib.v ~libname_of_archive ~pkg_name:pkg.name ~dir ~cmtidir:None
                ~all_lib_deps ~cmi_only_libs ~id_override:None
            in
            let libraries =
              List.filter
                (fun l ->
                  match l.archive_name with
                  | None -> true
                  | Some a -> Util.StringSet.mem a archives)
                libraries
            in
            Util.StringMap.update pkg.name
              (function
                | Some pkg ->
                    let libraries = libraries @ pkg.libraries in
                    Some { pkg with libraries }
                | None ->
                    let pkg_dir = pkg_dir packages_dir pkg.name in

                    let _, { Opam.docs; odoc_config; _ } =
                      List.find
                        (fun (pkg', _) ->
                          (* Logs.debug (fun m ->
                              m "Checking %s against %s" pkg.Opam.name pkg'.Opam.name); *)
                          pkg = pkg')
                        opam_map
                    in

                    let config =
                      match odoc_config with
                      | None -> Global_config.empty
                      | Some f -> Global_config.load f
                    in

                    let mlds, assets, _ = mk_mlds docs in
                    Some
                      {
                        name = pkg.name;
                        version = pkg.version;
                        libraries;
                        mlds;
                        assets;
                        selected = false;
                        remaps = [];
                        other_docs = [];
                        pkg_dir;
                        doc_dir = pkg_dir;
                        config;
                      })
              acc)
      archives_by_dir Util.StringMap.empty
  in
  let packages = Util.StringMap.bindings packages |> List.map snd in
  fix_missing_deps packages

let of_packages ~packages_dir packages =
  Logs.app (fun m -> m "Deciding which packages to build...");
  let deps =
    if packages = [] then Opam.all_opam_packages () else Opam.deps packages
  in

  let Ocamlfind.Db.{ libname_of_archive; cmi_only_libs; all_lib_deps; _ } =
    Ocamlfind.Db.create (Ocamlfind.all () |> Util.StringSet.of_list)
  in

  let opam_map, _opam_rmap = Opam.pkg_to_dir_map () in

  let ps =
    List.filter_map
      (fun pkg ->
        match
          List.find_opt
            (fun (pkg', _) -> pkg.Opam.name = pkg'.Opam.name)
            opam_map
        with
        | None ->
            Logs.warn (fun m ->
                m "Didn't find package %a in opam_map" Opam.pp pkg);
            None
        | x -> x)
      deps
  in

  let orig =
    List.filter_map
      (fun pkg ->
        List.find_opt (fun (pkg', _) -> pkg = pkg'.Opam.name) opam_map)
      packages
  in

  let all = orig @ ps in
  let all =
    List.sort_uniq
      (fun (a, _) (b, _) -> String.compare a.Opam.name b.Opam.name)
      all
  in

  Logs.app (fun m -> m "Performing module-level dependency analysis...");

  let packages =
    List.map
      (fun (pkg, files) ->
        let libraries =
          List.fold_left
            (fun acc dir ->
              Lib.v ~libname_of_archive ~pkg_name:pkg.Opam.name ~dir
                ~cmtidir:None ~all_lib_deps ~cmi_only_libs ~id_override:None
              @ acc)
            []
            (files.Opam.libs |> Fpath.Set.to_list)
        in
        let pkg_dir = pkg_dir packages_dir pkg.name in
        let config =
          match files.odoc_config with
          | None -> Global_config.empty
          | Some f -> Global_config.load f
        in
        let mlds, assets, _ = mk_mlds files.docs in
        let selected = List.mem pkg.name packages in
        let remaps =
          if selected then []
          else
            let local_pkg_path = Fpath.to_string (Fpath.to_dir_path pkg_dir) in
            let pkg_path =
              Printf.sprintf "https://ocaml.org/p/%s/%s/doc/" pkg.name
                pkg.version
            in
            let lib_paths =
              List.map
                (fun libty ->
                  let lib_name = libty.lib_name in
                  let local_lib_path =
                    Printf.sprintf "%s%s/" local_pkg_path lib_name
                  in
                  let lib_path = pkg_path in
                  (local_lib_path, lib_path))
                libraries
            in
            (local_pkg_path, pkg_path) :: lib_paths
        in

        {
          name = pkg.name;
          version = pkg.version;
          libraries;
          mlds;
          assets;
          selected;
          remaps;
          other_docs = [];
          pkg_dir;
          doc_dir = pkg_dir;
          config;
        })
      all
  in
  let res = fix_missing_deps packages in
  Logs.debug (fun m -> m "Packages: %a" Fmt.Dump.(list pp) res);
  res

let remap_virtual_interfaces duplicate_hashes pkgs =
  List.map
    (fun pkg ->
      {
        pkg with
        libraries =
          pkg.libraries
          |> List.map (fun lib ->
                 {
                   lib with
                   modules =
                     lib.modules
                     |> List.map (fun m ->
                            let m_intf =
                              if
                                Util.StringMap.mem m.m_intf.mif_hash
                                  duplicate_hashes
                                && Fpath.has_ext "cmt" m.m_intf.mif_path
                              then
                                match
                                  List.filter
                                    (fun intf ->
                                      Fpath.has_ext "cmti" intf.mif_path)
                                    (Util.StringMap.find m.m_intf.mif_hash
                                       duplicate_hashes)
                                with
                                | [ x ] -> x
                                | _ -> m.m_intf
                              else m.m_intf
                            in
                            { m with m_intf });
                 });
      })
    pkgs

let remap_virtual all =
  let virtual_check =
    let hashes =
      List.fold_left
        (fun acc pkg ->
          List.fold_left
            (fun acc lib ->
              List.fold_left
                (fun acc m ->
                  let hash = m.m_intf.mif_hash in
                  Util.StringMap.update hash
                    (function
                      | None -> Some [ m.m_intf ]
                      | Some l -> Some (m.m_intf :: l))
                    acc)
                acc lib.modules)
            acc pkg.libraries)
        Util.StringMap.empty all
    in
    Util.StringMap.filter (fun _hash intfs -> List.length intfs > 1) hashes
  in

  remap_virtual_interfaces virtual_check all
