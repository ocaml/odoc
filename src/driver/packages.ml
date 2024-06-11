(* Packages *)

type dep = string * Digest.t

type intf = {
  mif_odoc_file : Fpath.t;
  mif_odocl_file : Fpath.t;
  mif_parent_id : string;
  mif_hash : string;
  mif_path : Fpath.t;
  mif_deps : dep list;
}

let pp_intf fmt i = Format.fprintf fmt "intf: %a" Fpath.pp i.mif_path

type src_info = { src_path : Fpath.t; src_id : string }

type impl = {
  mip_odoc_file : Fpath.t;
  mip_odocl_file : Fpath.t;
  mip_parent_id : string;
  mip_path : Fpath.t;
  mip_src_info : src_info option;
}

let pp_impl fmt i = Format.fprintf fmt "impl: %a" Fpath.pp i.mip_path

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
}

type mld = {
  mld_odoc_file : Fpath.t;
  mld_odocl_file : Fpath.t;
  mld_parent_id : string;
  mld_path : Fpath.t;
  mld_deps : Fpath.t list;
}

let pp_mld fmt m = Format.fprintf fmt "%a" Fpath.pp m.mld_path

type libty = {
  lib_name : string;
  dir : Fpath.t;
  odoc_dir : Fpath.t;
  archive_name : string;
  modules : modulety list;
}

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  other_docs : Fpath.Set.t;
}

module Module = struct
  type t = modulety

  let pp ppf (t : t) =
    Fmt.pf ppf "name: %s@.intf: %a@.impl: %a@.hidden: %b@." t.m_name Fpath.pp
      t.m_intf.mif_path (Fmt.option pp_impl) t.m_impl t.m_hidden

  let is_hidden name = Astring.String.is_infix ~affix:"__" name

  let vs pkg_name lib_name dir modules =
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
        let mif_parent_id = Printf.sprintf "%s/lib/%s" pkg_name lib_name in
        let mif_odoc_file =
          Fpath.(
            v mif_parent_id
            // set_ext "odoc" (v (String.uncapitalize_ascii m_name)))
        in
        let mif_odocl_file = Fpath.(set_ext "odocl" mif_odoc_file) in
        match Odoc.compile_deps mif_path with
        | Ok { digest; deps } ->
            {
              mif_odoc_file;
              mif_odocl_file;
              mif_parent_id;
              mif_hash = digest;
              mif_path;
              mif_deps = deps;
            }
        | Error _ -> failwith "bad deps"
      in
      let mk_impl mip_path =
        let mip_parent_id = Printf.sprintf "%s/lib/%s" pkg_name lib_name in
        let mip_odoc_file =
          Fpath.(
            v mip_parent_id
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
              let src_id =
                Printf.sprintf "%s/src/%s/%s" pkg_name lib_name src_name
              in
              Some { src_path; src_id }
        in
        { mip_odoc_file; mip_odocl_file; mip_parent_id; mip_src_info; mip_path }
      in
      let state = (exists "cmt", exists "cmti") in

      let m_hidden = is_hidden m_name in
      try
        let m_intf, m_impl =
          match state with
          | Some cmt, Some cmti -> (mk_intf cmti, Some (mk_impl cmt))
          | Some cmt, None -> (mk_intf cmt, Some (mk_impl cmt))
          | None, Some cmti -> (mk_intf cmti, None)
          | None, None ->
              Logs.err (fun m -> m "No files for module: %s" m_name);
              failwith "no files"
        in
        Some { m_name; m_intf; m_impl; m_hidden }
      with _ ->
        Logs.err (fun m -> m "Error processing module %s. Ignoring." m_name);
        None
    in

    Eio.Fiber.List.filter_map mk modules
end

module Lib = struct
  exception Unknown_archive of string

  let v libname_of_archive pkg_name dir =
    Logs.debug (fun m ->
        m "Classifying dir %a for package %s" Fpath.pp dir pkg_name);
    let results = Odoc.classify dir in
    List.filter_map
      (fun (archive_name, modules) ->
        try
          let lib_name =
            try Util.StringMap.find archive_name libname_of_archive
            with Not_found -> raise (Unknown_archive archive_name)
          in
          let modules = Module.vs pkg_name lib_name dir modules in
          let odoc_dir =
            List.hd modules |> fun m ->
            m.m_intf.mif_odoc_file |> Fpath.split_base |> fst
          in
          Some { lib_name; dir; odoc_dir; archive_name; modules }
        with
        | Unknown_archive x ->
            Logs.debug (fun m ->
                m
                  "Unable to determine library in package '%s' to which \
                   archive '%s' belongs"
                  pkg_name x);
            Logs.debug (fun m ->
                m "These are the archives I know about: [%a]"
                  Fmt.(list ~sep:sp string)
                  (Util.StringMap.bindings libname_of_archive |> List.map fst));
            None
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

let of_libs libs =
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
            let id = Fpath.(v pkg_name / "doc" // rel_path) in
            let mld_parent_id =
              Format.asprintf "%a" Fpath.pp
                (id |> Fpath.parent |> Fpath.rem_empty_seg)
            in
            let page_name = Fpath.(rem_ext mld_path |> filename) in
            let odoc_file =
              Fpath.(v mld_parent_id / ("page-" ^ page_name ^ ".odoc"))
            in
            let odocl_file = Fpath.(set_ext "odocl" odoc_file) in
            let mld_deps = List.map (fun l -> l.odoc_dir) libraries in
            {
              mld_odoc_file = odoc_file;
              mld_odocl_file = odocl_file;
              mld_parent_id;
              mld_path;
              mld_deps;
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
          let libraries = Lib.v libname_of_archive pkg.name dir in
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
                  Some
                    {
                      name = pkg.name;
                      version = pkg.version;
                      libraries;
                      mlds;
                      other_docs;
                    })
            acc)
    dirs Util.StringMap.empty

type set = t Util.StringMap.t
