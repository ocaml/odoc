(* Packages *)

type dep = string * Digest.t

type id = Odoc.id

type intf = {
  mif_odoc_file : Fpath.t; (* Relative to [odoc] dir *)
  mif_odocl_file : Fpath.t; (* Relative to [odocl] dir *)
  mif_parent_id : id;
  mif_hash : string;
  mif_path : Fpath.t; (* Relative to cwd or absolute path *)
  mif_deps : dep list;
}

let pp_intf fmt i = Format.fprintf fmt "intf: %a" Fpath.pp i.mif_path

type src_info = { src_path : Fpath.t; src_id : id }

type impl = {
  mip_odoc_file : Fpath.t; (* Relative to [odoc] dir *)
  mip_odocl_file : Fpath.t; (* Relative to [odocl] dir *)
  mip_parent_id : id;
  mip_path : Fpath.t; (* Relative to cwd or absolute path *)
  mip_src_info : src_info option;
}

let pp_impl fmt i = Format.fprintf fmt "impl: %a" Fpath.pp i.mip_path

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
  m_pkg_dir : Fpath.t (* The 'top dir' of a package, relative to [_odoc] or [_html] *)
}

type mld = {
  mld_odoc_file : Fpath.t; (* Relative to [odoc] dir *)
  mld_odocl_file : Fpath.t; (* Relative to [odocl] dir *)
  mld_parent_id : id;
  mld_path : Fpath.t; (* Absolute or relative to cwd *)
  mld_deps : Fpath.t list;
  mld_pkg_dir : Fpath.t; (* The 'top dir' of a package, relative to [_odoc] or [_html] *)
}

let pp_mld fmt m = Format.fprintf fmt "%a" Fpath.pp m.mld_path

type libty = {
  lib_name : string;
  dir : Fpath.t;
  odoc_dir : Fpath.t; (* Relative to [odoc] dir *)
  archive_name : string;
  modules : modulety list;
}

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  mld_odoc_dir : Fpath.t; (* Relative to [odoc] dir *)
  pkg_dir : Fpath.t;
  other_docs : Fpath.Set.t;
}

let maybe_prepend_top top_dir dir =
  match top_dir with
  | None -> dir
  | Some d -> Fpath.(d // dir)

let pkg_dir top_dir pkg_name =
  maybe_prepend_top top_dir Fpath.(v pkg_name)

let parent_of_lib pkg_dir lib_name =
  Fpath.(pkg_dir / "lib" / lib_name)

let parent_of_pkg pkg_dir =
  Fpath.(pkg_dir / "doc")

let parent_of_src pkg_dir lib_name =
  Fpath.(pkg_dir / "src" / lib_name)
  
module Module = struct
  type t = modulety

  let pp ppf (t : t) =
    Fmt.pf ppf "name: %s@.intf: %a@.impl: %a@.hidden: %b@." t.m_name Fpath.pp
      t.m_intf.mif_path (Fmt.option pp_impl) t.m_impl t.m_hidden

  let is_hidden name = Astring.String.is_infix ~affix:"__" name

  let vs pkg_dir lib_name dir modules =
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
        let mif_parent_id = parent_of_lib pkg_dir lib_name in
        let mif_odoc_file =
          Fpath.(
            mif_parent_id
            // set_ext "odoc" (v (String.uncapitalize_ascii m_name)))
        in
        let mif_odocl_file = Fpath.(set_ext "odocl" mif_odoc_file) in
        match Odoc.compile_deps mif_path with
        | Ok { digest; deps } ->
            {
              mif_odoc_file;
              mif_odocl_file;
              mif_parent_id = Odoc.id_of_fpath mif_parent_id;
              mif_hash = digest;
              mif_path;
              mif_deps = deps;
            }
        | Error _ -> failwith "bad deps"
      in
      let mk_impl mip_path =
        let mip_parent_id = parent_of_lib pkg_dir lib_name in
        let mip_odoc_file =
          Fpath.(
            mip_parent_id
            // add_ext "odoc" (v ("impl-" ^ String.uncapitalize_ascii m_name)))
        in
        let mip_odocl_file = Fpath.(set_ext "odocl" mip_odoc_file) in
        let mip_src_info =
          match Ocamlobjinfo.get_source mip_path with
          | None ->
              Logs.debug (fun m -> m "No source found for module %s" m_name);
              None
          | Some src_path ->
              Logs.debug (fun m ->
                  m "Found source file %a for %s" Fpath.pp src_path m_name);
              let src_name = Fpath.filename src_path in
              let src_id = Fpath.(parent_of_src pkg_dir lib_name / src_name) |> Odoc.id_of_fpath in
              Some { src_path; src_id }
        in
        { mip_odoc_file; mip_odocl_file; mip_parent_id = Odoc.id_of_fpath mip_parent_id; mip_src_info; mip_path }
      in
      let state = (exists "cmt", exists "cmti") in

      let m_hidden = is_hidden m_name in
      try
        let r (m_intf, m_impl) =
          Some { m_name; m_intf; m_impl; m_hidden; m_pkg_dir = pkg_dir }
        in
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

  let v pkg_dir libname_of_archive pkg_name dir =
    Logs.debug (fun m ->
        m "Classifying dir %a for package %s" Fpath.pp dir pkg_name);
    let results = Odoc.classify dir in
    Logs.debug (fun m ->
      m "Got %d lines" (List.length results));
    List.filter_map
      (fun (archive_name, modules) ->
        try
          let lib_name =
            try Util.StringMap.find archive_name libname_of_archive
            with Not_found ->
              Logs.debug (fun m ->
                m
                  "Unable to determine library in package '%s' to which \
                   archive '%s' belongs"
                  pkg_name archive_name);
            Logs.debug (fun m ->
                m "These are the archives I know about: [%a]"
                  Fmt.(list ~sep:sp string)
                  (Util.StringMap.bindings libname_of_archive |> List.map fst));
            Logs.debug (fun m ->
                m "Defaulting to name of library: %s" archive_name;
                );
            archive_name
          in
          let modules = Module.vs pkg_dir lib_name dir modules in
          let odoc_dir =
            parent_of_lib pkg_dir lib_name
          in
          Some { lib_name; dir; odoc_dir; archive_name; modules }
        with
        | _ ->
            Logs.err (fun m ->
                m "Error processing library %s. Ignoring." archive_name);
            None)
      results

  let pp ppf t =
    Fmt.pf ppf "path: %a archive: %a modules: [@[<hov 2>@,%a@]@,]" Fpath.pp
      t.dir Fmt.string t.archive_name
      Fmt.(list ~sep:sp Module.pp)
      t.modules
end

let pp ppf t =
  Fmt.pf ppf "name: %s@.version: %s@.libraries: [@[<hov 2>@,%a@]@,]" t.name
    t.version
    Fmt.(list ~sep:sp Lib.pp)
    t.libraries

let of_libs packages_dir libs =
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
  let mk_mlds pkg_name libraries odoc_pages =
    let prefix = Fpath.(v (Opam.prefix ()) / "doc" / pkg_name / "odoc-pages") in
    Fpath.Set.fold
      (fun mld_path acc ->
        let rel_path = Fpath.rem_prefix prefix mld_path in
        match rel_path with
        | None -> acc
        | Some rel_path ->
            let pkg_dir = (pkg_dir packages_dir pkg_name) in
            let id = Fpath.(parent_of_pkg pkg_dir // rel_path) in 
            let mld_parent_id =
                (id |> Fpath.parent |> Fpath.rem_empty_seg)
            in
            let page_name = Fpath.(rem_ext mld_path |> filename) in
            let odoc_file =
              Fpath.(mld_parent_id / ("page-" ^ page_name ^ ".odoc"))
            in
            let odocl_file = Fpath.(set_ext "odocl" odoc_file) in
            let mld_deps = List.map (fun l -> l.odoc_dir) libraries in
            {
              mld_odoc_file = odoc_file;
              mld_odocl_file = odocl_file;
              mld_parent_id = Odoc.id_of_fpath mld_parent_id;
              mld_path;
              mld_deps;
              mld_pkg_dir = pkg_dir;
            }
            :: acc)
      odoc_pages []
  in
  let update_mlds mlds libraries =
    List.map
      (fun mld ->
        let mld_deps = List.map (fun l -> l.odoc_dir) libraries in
        { mld with mld_deps })
      mlds
  in
  Fpath.Map.fold
    (fun dir archives acc ->
      match Fpath.Map.find dir rmap with
      | None ->
          Logs.debug (fun m -> m "No package for dir %a\n%!" Fpath.pp dir);
          acc
      | Some pkg ->
          let pkg_dir = pkg_dir packages_dir pkg.name in

          let libraries = Lib.v pkg_dir libname_of_archive pkg.name dir in
          let libraries =
            List.filter
              (fun l -> Util.StringSet.mem l.archive_name archives)
              libraries
          in
          let pkg', { Opam.odoc_pages; other_docs; _ } =
            List.find
              (fun (pkg', _) ->
                (* Logs.debug (fun m ->
                    m "Checking %s against %s" pkg.Opam.name pkg'.Opam.name); *)
                pkg = pkg')
              map
          in
          let mlds = mk_mlds pkg'.name libraries odoc_pages in
          Logs.debug (fun m ->
              m "%d mlds for package %s (from %d odoc_pages)" (List.length mlds)
                pkg.name
                (Fpath.Set.cardinal odoc_pages));
          Util.StringMap.update pkg.name
            (function
              | Some pkg ->
                  let libraries = libraries @ pkg.libraries in
                  Some
                    {
                      pkg with
                      libraries;
                      mlds = update_mlds pkg.mlds libraries;
                    }
              | None ->
                  let mld_odoc_dir = parent_of_pkg pkg_dir in
                  Some
                    {
                      name = pkg.name;
                      version = pkg.version;
                      libraries;
                      mlds;
                      mld_odoc_dir;
                      other_docs;
                      pkg_dir;
                    })
            acc)
    dirs Util.StringMap.empty

type set = t Util.StringMap.t
