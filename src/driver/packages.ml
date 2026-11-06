(* Packages *)

type dep = string * Digest.t

type intf = { mif_hash : string; mif_path : Fpath.t; mif_deps : dep list }

let pp_intf fmt (i : intf) =
  Format.fprintf fmt "@[<hov>{@,mif_hash: %s;@,mif_path: %a;@,mif_deps: %a@,}@]"
    i.mif_hash Fpath.pp i.mif_path
    (Fmt.Dump.list (Fmt.Dump.pair Fmt.string Fmt.string))
    i.mif_deps

type src_info = { src_path : Fpath.t }

let pp_src_info fmt i =
  Format.fprintf fmt "@[<hov>{@,src_path: %a@,}@]" Fpath.pp i.src_path

type impl = { mip_path : Fpath.t; mip_src_info : src_info option }

let pp_impl fmt i =
  Format.fprintf fmt "@[<hov>{@,mip_path: %a;@,mip_src_info: %a@,}@]" Fpath.pp
    i.mip_path
    (Fmt.Dump.option pp_src_info)
    i.mip_src_info

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
  rel_dir : Fpath.t;
  archive_name : string option;
  lib_deps : Util.StringSet.t;
  modules : modulety list;
}

let pp_libty fmt l =
  Format.fprintf fmt
    "@[<hov>{@,\
     lib_name: %s;@,\
     dir: %a;@,\
     rel_dir: %a;@,\
     archive_name: %a;@,\
     lib_deps: %a;@,\
     modules: %a@,\
     }@]"
    l.lib_name Fpath.pp l.dir Fpath.pp l.rel_dir
    (Fmt.Dump.option Fmt.string)
    l.archive_name
    (Fmt.list ~sep:Fmt.comma Fmt.string)
    (Util.StringSet.elements l.lib_deps)
    (Fmt.Dump.list pp_modulety)
    l.modules

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  other_docs : md list;
  pkg_dir : Fpath.t;
  doc_dir : Fpath.t;
  config : Global_config.t;
  doc_deps : string list;
}

(* The documentation of a package this run does not render is on ocaml.org,
   so a link into it is rewritten to point there. Every library of the
   package goes to the same page as the package itself. *)
let remaps t =
  let local_pkg_path = Fpath.to_string (Fpath.to_dir_path t.pkg_dir) in
  let pkg_path =
    Printf.sprintf "https://ocaml.org/p/%s/%s/doc/" t.name t.version
  in
  let lib_paths =
    List.map
      (fun (lib : libty) ->
        (Printf.sprintf "%s%s/" local_pkg_path lib.lib_name, pkg_path))
      t.libraries
  in
  (local_pkg_path, pkg_path) :: lib_paths

let pp fmt t =
  Format.fprintf fmt
    "@[<hov>{@,\
     name: %s;@,\
     version: %s;@,\
     libraries: %a;@,\
     mlds: %a;@,\
     assets: %a;@,\
     other_docs: %a;@,\
     pkg_dir: %a@,\
     }@]"
    t.name t.version (Fmt.Dump.list pp_libty) t.libraries (Fmt.Dump.list pp_mld)
    t.mlds (Fmt.Dump.list pp_asset) t.assets (Fmt.Dump.list pp_md) t.other_docs
    Fpath.pp t.pkg_dir

let maybe_prepend_top top_dir dir =
  match top_dir with None -> dir | Some d -> Fpath.(d // dir)

let pkg_dir top_dir pkg_name = maybe_prepend_top top_dir Fpath.(v pkg_name)

module Module = struct
  type t = modulety

  let pp ppf (t : t) =
    Fmt.pf ppf "name: %s@.intf: %a@.impl: %a@.hidden: %b@." t.m_name Fpath.pp
      t.m_intf.mif_path (Fmt.option pp_impl) t.m_impl t.m_hidden

  let is_hidden name = Astring.String.is_infix ~affix:"__" name

  let vs dir modules =
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
        let mip_src_info =
          match Ocamlobjinfo.get_source mip_path [ dir ] with
          | None ->
              Logs.debug (fun m -> m "No source found for module %s" m_name);
              None
          | Some src_path ->
              Logs.debug (fun m ->
                  m "Found source file %a for %s" Fpath.pp src_path m_name);
              Some { src_path }
        in
        { mip_src_info; mip_path }
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
  (* The object directory of a library -- where its [cmi]/[cmt] files live --
     relative to the first of [roots] that contains it. The [.odoc] files of
     the library are written to the same relative path below the odoc
     directory, so that the directory structure of compiled documentation
     mirrors that of the compiled objects. Libraries installed in a single
     directory (e.g. the [compiler-libs.*] family) therefore share an odoc
     directory too, and the [-L] of any one of them finds every one of them,
     just as the compiler's [-I] does. *)
  let rel_dir ~roots dir =
    let dir = Fpath.normalize dir in
    match List.find_map (fun root -> Fpath.rem_prefix root dir) roots with
    | Some rel -> rel
    | None -> (
        if Fpath.is_rel dir then dir
        else
          (* Not below any root: drop the leading '/' and use the path as is. *)
          match Fpath.segs dir with
          | "" :: segs -> Fpath.v (String.concat "/" segs)
          | _ -> dir)

  (* What a library requires, as its META declared it. A library found
     without one, which is how the compiler's own libraries are found in a
     switch with no ocamlfind, declares nothing; it is still compiled against
     the standard library, and saying so is what lets its modules resolve the
     signatures they are constrained by. *)
  let lib_deps_of installed lib_name =
    Util.StringSet.add "stdlib" (Installed.requires installed lib_name)

  let handle_virtual_lib ~roots ~installed ~dir ~lib_name =
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
    let modules = Module.vs dir modules in
    let lib_deps = lib_deps_of installed lib_name in
    let rel_dir = rel_dir ~roots dir in
    [ { lib_name; archive_name = None; modules; lib_deps; dir; rel_dir } ]

  let v ~roots ~installed ~pkg_name ~dir =
    Logs.debug (fun m ->
        m "Classifying dir %a for package %s" Fpath.pp dir pkg_name);
    let results = Odoc.classify [ dir ] in
    let rel_dir = rel_dir ~roots dir in
    match List.length results with
    | 0 -> (
        match Installed.no_archive_lib installed dir with
        | None -> []
        | Some lib_name -> handle_virtual_lib ~roots ~installed ~dir ~lib_name)
    | _ ->
        Logs.debug (fun m -> m "Got %d lines" (List.length results));
        let of_libraries =
          List.filter_map
            (fun (archive_name, modules) ->
              match
                Installed.lib_of_archive installed Fpath.(dir / archive_name)
              with
              | Some lib_name -> Some (lib_name, archive_name, modules)
              | None ->
                  (* An archive no META declares, as the compiler's ocamldoc
                     directory holds odoc_info.cma. Its modules are still
                     installed for use, so it is documented, as a library
                     named after it. *)
                  Logs.info (fun m ->
                      m
                        "No library declares archive %a: documenting it as \
                         library %s"
                        Fpath.pp
                        Fpath.(dir / archive_name)
                        archive_name);
                  Some (archive_name, archive_name, modules))
            results
        in
        (* An archive may bundle another's modules: ocamloptcomp holds all of
           ocamlmiddleend's. [odoc classify] says so, reporting the module
           under both, but a module is documented once, so each is given to
           the largest library that holds it. The bundling archive is the one
           that ships as a library people can depend on -- findlib declares
           compiler-libs.optcomp and has no name at all for the middle end --
           and the smaller archive is a component of it. Without a rule the
           two libraries write the same file and whichever ran last decides,
           which lost every page of the loser. *)
        let owner =
          List.fold_left
            (fun acc (lib_name, _, modules) ->
              let size = List.length modules in
              List.fold_left
                (fun acc m ->
                  match Util.StringMap.find_opt m acc with
                  | Some (_, best) when best >= size -> acc
                  | _ -> Util.StringMap.add m (lib_name, size) acc)
                acc modules)
            Util.StringMap.empty of_libraries
        in
        List.filter_map
          (fun (lib_name, archive_name, modules) ->
            let modules =
              List.filter
                (fun m ->
                  match Util.StringMap.find_opt m owner with
                  | Some (owner, _) -> owner = lib_name
                  | None -> true)
                modules
            in
            match modules with
            | [] ->
                Logs.info (fun m ->
                    m
                      "Every module of library %s is also in a larger library \
                       in the same directory, which is the one that ships; it \
                       has nothing of its own to document"
                      lib_name);
                None
            | _ ->
                let modules = Module.vs dir modules in
                let lib_deps = lib_deps_of installed lib_name in
                Some
                  {
                    lib_name;
                    archive_name = Some archive_name;
                    modules;
                    lib_deps;
                    dir;
                    rel_dir;
                  })
          of_libraries

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

let of_packages ~installed ~packages_dir packages =
  Logs.app (fun m -> m "Deciding which packages to build...");
  let deps =
    if packages = [] then Opam.all_opam_packages () else Opam.deps packages
  in

  let opam_map = Installed.package_files installed in
  let roots = Opam.install_roots () in

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
  (* opam does not count an optional dependency as required, but a library
     compiled against one needs its docs as much as any other (D1 in
     doc/scope_model.mld). So add, until nothing changes, the package that
     installed each library the run's libraries require. *)
  let all =
    let in_run all lib =
      match Installed.package installed lib with
      | Some p -> List.exists (fun (p', _) -> p'.Opam.name = p) all
      | None -> false
    in
    let rec close all =
      let required =
        List.fold_left
          (fun acc (l : Installed.lib) ->
            if in_run all l.name then Util.StringSet.union l.requires acc
            else acc)
          Util.StringSet.empty (Installed.libs installed)
      in
      let added =
        Util.StringSet.fold
          (fun lib acc ->
            match Installed.package installed lib with
            | Some p when not (in_run all lib || List.mem p acc) -> p :: acc
            | _ -> acc)
          required []
      in
      match
        List.filter_map
          (fun p -> List.find_opt (fun (p', _) -> p'.Opam.name = p) opam_map)
          added
      with
      | [] -> close_doc_deps all
      | more ->
          Logs.debug (fun m ->
              m "Adding the packages of required libraries: %a"
                Fmt.(list ~sep:comma Opam.pp)
                (List.map fst more));
          close (all @ more)
    (* ocaml-docs-ci installs a package's doc deps for its documentation, and
       what they depend on, so the run documents them too. *)
    and close_doc_deps all =
      let in_all name = List.exists (fun (p, _) -> p.Opam.name = name) all in
      let wanted =
        List.concat_map (fun (p, _) -> Opam.doc_deps p) all
        |> List.filter (fun n -> not (in_all n))
        |> List.sort_uniq String.compare
      in
      let available =
        List.filter
          (fun n -> List.exists (fun (p, _) -> p.Opam.name = n) opam_map)
          wanted
      in
      match available with
      | [] -> all
      | names ->
          let more =
            List.filter_map
              (fun (p : Opam.package) ->
                if in_all p.name then None
                else
                  List.find_opt (fun (p', _) -> p'.Opam.name = p.name) opam_map)
              (List.map (fun name -> { Opam.name; version = "" }) names
              @ Opam.deps names)
            |> List.sort_uniq (fun (a, _) (b, _) ->
                   String.compare a.Opam.name b.Opam.name)
          in
          Logs.debug (fun m ->
              m "Adding the doc deps of the run's packages: %a"
                Fmt.(list ~sep:comma Opam.pp)
                (List.map fst more));
          close (all @ more)
    in
    close all
  in

  Logs.app (fun m -> m "Performing module-level dependency analysis...");

  let packages =
    List.map
      (fun (pkg, files) ->
        let libraries =
          List.fold_left
            (fun acc dir ->
              Lib.v ~roots ~installed ~pkg_name:pkg.Opam.name ~dir @ acc)
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
        let doc_deps = Opam.doc_deps pkg in
        (* The config says what the scope is, and the opam file what is
           installed for the docs and built first. A package named only in the
           config is not installed by ocaml-docs-ci, and not waited for here.
           Said for the packages the run was asked for, as their warnings are. *)
        (match config.deps.packages with
        | _ when not (List.mem pkg.name packages) -> ()
        | [] -> ()
        | named ->
            let deps =
              List.map
                (fun (p : Opam.package) -> p.name)
                (Opam.deps [ pkg.name ])
            in
            List.iter
              (fun p ->
                if not (List.mem p doc_deps || List.mem p deps || p = pkg.name)
                then
                  Logs.warn (fun m ->
                      m
                        "Package %s's odoc-config.sexp names %s, which its \
                         opam file lists neither as a dependency nor in \
                         x-extra-doc-deps"
                        pkg.name p))
              named);
        {
          name = pkg.name;
          version = pkg.version;
          libraries;
          mlds;
          assets;
          other_docs = [];
          pkg_dir;
          doc_dir = pkg_dir;
          config;
          doc_deps;
        })
      all
  in
  Logs.debug (fun m -> m "Packages: %a" Fmt.Dump.(list pp) packages);
  packages

(* A virtual library's implementations ship [.cmt] files only. Their
   interface is the virtual library's [.cmti], which has the same digest, so
   that is the interface they are given. *)
let remap_virtual all =
  let cmtis =
    List.fold_left
      (fun acc pkg ->
        List.fold_left
          (fun acc lib ->
            List.fold_left
              (fun acc m ->
                if Fpath.has_ext "cmti" m.m_intf.mif_path then
                  Util.StringMap.add_to_list m.m_intf.mif_hash m.m_intf acc
                else acc)
              acc lib.modules)
          acc pkg.libraries)
      Util.StringMap.empty all
  in
  let cmti_of hash =
    match Util.StringMap.find_opt hash cmtis with
    | Some [ x ] -> Some x
    | _ -> None
  in
  let remap m =
    if Fpath.has_ext "cmt" m.m_intf.mif_path then
      match cmti_of m.m_intf.mif_hash with
      | Some m_intf -> { m with m_intf }
      | None -> m
    else m
  in
  List.map
    (fun pkg ->
      {
        pkg with
        libraries =
          List.map
            (fun lib -> { lib with modules = List.map remap lib.modules })
            pkg.libraries;
      })
    all
