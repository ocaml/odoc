open Odoc_unit

type indices_style = Voodoo | Normal of { toplevel_content : string option }

(* Everything known about libraries and packages: those being built, and any
   other that is installed ({!Installed}), and the layout convention (see
   [Odoc_unit.lib_obj_dir]). Paths are absolute. *)
type known = {
  installed : Installed.t;
  odoc_root : Fpath.t;  (** the odoc directory *)
  packages : Util.StringSet.t;  (** the packages being built *)
  building : Util.StringSet.t;  (** the libraries being built *)
  lib_dir : string -> Fpath.t option;
      (** library -> directory of its [.odoc] files *)
  lib_pkg : string -> string option;  (** library -> package providing it *)
  requires : string -> Util.StringSet.t;  (** direct META requires *)
}

let known ~installed ~odoc_dir (pkgs : Packages.t list) =
  let own =
    List.fold_left
      (fun acc (pkg : Packages.t) ->
        List.fold_left
          (fun acc (lib : Packages.libty) ->
            (* The first package to claim a name keeps it, as in [libs_of],
               so that a name means one directory (D7). *)
            if Util.StringMap.mem lib.lib_name acc then acc
            else Util.StringMap.add lib.lib_name (pkg, lib) acc)
          acc pkg.libraries)
      Util.StringMap.empty pkgs
  in
  let building =
    Util.StringMap.fold
      (fun l _ acc -> Util.StringSet.add l acc)
      own Util.StringSet.empty
  in
  let packages =
    List.fold_left
      (fun acc (p : Packages.t) -> Util.StringSet.add p.name acc)
      Util.StringSet.empty pkgs
  in
  let roots = lazy (Opam.install_roots ()) in
  let own_or f g lib =
    match Util.StringMap.find_opt lib own with
    | Some (pkg, l) -> f pkg l
    | None -> g lib
  in
  let lib_dir =
    own_or
      (fun _ l -> Some Fpath.(odoc_dir // lib_obj_dir l))
      (fun lib ->
        Option.map
          (fun (l : Installed.lib) ->
            Fpath.(
              odoc_dir // Packages.Lib.rel_dir ~roots:(Lazy.force roots) l.dir))
          (Installed.find installed lib))
  in
  let lib_pkg =
    own_or
      (fun (pkg : Packages.t) _ -> Some pkg.name)
      (Installed.package installed)
  in
  let requires =
    own_or
      (fun _ (l : Packages.libty) -> l.lib_deps)
      (Installed.requires installed)
  in
  {
    installed;
    odoc_root = odoc_dir;
    packages;
    building;
    lib_dir;
    lib_pkg;
    requires;
  }

(* Where a package's pages are, by convention. *)
let pkg_pages_dir known name = Fpath.(known.odoc_root / "doc" / name)

(* What unit construction needs to know: the world of libraries and packages,
   where the output goes, and whether unselected packages are remapped. *)
type ctx = {
  known : known;
  dirs : dirs;
  remap : bool;
  selected : Util.StringSet.t;
}

(* The packages the run was asked for, rather than pulled in as
   dependencies. Their warnings are reported and their pages rendered. *)
let selected ctx (pkg : Packages.t) = Util.StringSet.mem pkg.name ctx.selected

(* Walk [names] and, transitively, the requires of those that do not satisfy
   [keep], collecting those that do. A library the caller does not keep is
   thereby stood in for by its requires. That is how an alias is followed:
   [num] forwards to [num.core], [threads.posix] to [threads]. *)
let close known ~keep names =
  let rec go seen acc = function
    | [] -> acc
    | l :: todo when Util.StringSet.mem l seen -> go seen acc todo
    | l :: todo ->
        let seen = Util.StringSet.add l seen in
        if keep l then go seen (Util.StringSet.add l acc) todo
        else go seen acc (Util.StringSet.elements (known.requires l) @ todo)
  in
  go Util.StringSet.empty Util.StringSet.empty (Util.StringSet.elements names)

(* The libraries a list of requires actually names, among those we know the
   directory of. *)
let resolve known names =
  close known ~keep:(fun l -> Option.is_some (known.lib_dir l)) names

(* A library and its transitive dependencies: what it requires and, in turn,
   what those require. This is what the compiler saw when building it. *)
let with_transitive_deps known lib =
  let rec go acc = function
    | [] -> acc
    | l :: todo when Util.StringSet.mem l acc -> go acc todo
    | l :: todo ->
        let deps = resolve known (known.requires l) in
        go (Util.StringSet.add l acc) (Util.StringSet.elements deps @ todo)
  in
  go Util.StringSet.empty [ lib ]

(* Each library with the directory of its [.odoc] files, for [-L]. [odoc
   compile] writes the names into the units, so that a later [odoc link]
   resolving into a unit knows which libraries it could see. *)
let named_dirs_of known libs =
  Util.StringSet.fold
    (fun l acc ->
      match known.lib_dir l with Some d -> (l, d) :: acc | None -> acc)
    libs []
  |> List.sort_uniq compare

(* A library with no modules of its own: one with no archive, such as [seq],
   [bytes], or an alias such as [threads.posix] or [mtime.clock.os], or one
   whose installed directory holds no [.cmi] file, such as [stdlib-shims].
   Nothing documents it, so its [.odoc] directory may never exist. A unit
   compiled against it still records it, so a link reading that unit must be
   given it (D6), and it is, at no cost. *)
let without_modules known lib =
  match Installed.find known.installed lib with
  | None -> false
  | Some { archives = []; _ } -> true
  | Some { dir; _ } -> (
      match Bos.OS.Dir.contents ~rel:true dir with
      | Ok files -> not (List.exists (Fpath.has_ext ".cmi") files)
      | Error _ -> false)

(* The reference scope of a package, shared by all its units. [-L] is each of
   its own libraries and each library its [odoc-config.sexp] names, directly or
   through a named package, all with their transitive dependencies: a page's
   reference can reach a unit of any of those libraries, and linking it needs
   every library that unit recorded (D6 in doc/scope_model.mld). [-P] is the
   package itself, the packages providing those libraries, and the packages
   named in the config file. *)
let scope_of known (pkg : Packages.t) : scope =
  let { Global_config.deps = { packages = cfg_pkgs; libraries = cfg_libs } } =
    pkg.config
  in
  (* The libraries of a package the config names: those this run builds, or,
     for a package an earlier run built (voodoo mode builds one package per
     run), those its installed files hold. *)
  let libs_of_pkg p =
    let in_run =
      Util.StringSet.filter
        (fun lib -> known.lib_pkg lib = Some p)
        known.building
    in
    if Util.StringSet.mem p known.packages then in_run
    else
      Util.StringSet.union in_run
        (Util.StringSet.of_list (Installed.libs_of_package known.installed p))
  in
  let named =
    List.fold_left
      (fun acc (lib : Packages.libty) -> Util.StringSet.add lib.lib_name acc)
      (Util.StringSet.of_list cfg_libs)
      pkg.libraries
  in
  let named =
    List.fold_left
      (fun acc p -> Util.StringSet.union (libs_of_pkg p) acc)
      named cfg_pkgs
  in
  let named =
    Util.StringSet.fold
      (fun lib acc -> Util.StringSet.union (with_transitive_deps known lib) acc)
      named Util.StringSet.empty
  in
  (* A library is worth naming as a root if it is being built in this run or
     was built before, that is if its directory exists already. Otherwise it
     stands for the libraries it requires: an alias such as [threads.posix]
     for an in-run [threads], or an optional dependency that is not
     documented at all, which then contributes nothing. *)
  let built_or_building ~building name dir =
    Util.StringSet.mem name building || Bos.OS.Dir.exists dir = Ok true
  in
  let libs =
    close known
      ~keep:(fun l ->
        match known.lib_dir l with
        | Some dir ->
            built_or_building ~building:known.building l dir
            || without_modules known l
        | None -> false)
      named
  in
  let pkgs =
    Util.StringSet.fold
      (fun lib acc ->
        match known.lib_pkg lib with
        | Some p -> Util.StringSet.add p acc
        | None -> acc)
      libs
      (Util.StringSet.add pkg.name (Util.StringSet.of_list cfg_pkgs))
  in
  let lib_roots = named_dirs_of known libs in
  let page_roots =
    Util.StringSet.fold
      (fun p acc ->
        let dir = pkg_pages_dir known p in
        if built_or_building ~building:known.packages p dir then (p, dir) :: acc
        else acc)
      pkgs []
  in
  { page_roots; lib_roots }

(* The driver writes no pages of its own for a package it only remaps. *)
let writes_pages ctx (pkg : Packages.t) = (not ctx.remap) || selected ctx pkg

let index_of ~dirs (pkg : Packages.t) : index =
  {
    index_file = Fpath.(dirs.index_dir / pkg.name / Odoc.index_filename);
    sidebar_file = Fpath.(dirs.index_dir / pkg.name / Odoc.sidebar_filename);
    html_dir = doc_dir pkg;
  }

(* [rel_dir] is the unit's parent id, which fixes its identifier and URL.
   [obj_dir] is where its [.odoc]/[.odocl] files go; for modules this is the
   library's mirrored object directory, for pages it is [rel_dir]. *)
let make_unit ctx ~name ~kind ~rel_dir ~obj_dir ~input_file ~enable_warnings
    ~to_output ~stash_input : _ t =
  let to_output = to_output || not ctx.remap in
  (* If we haven't got active remapping, we output everything *)
  let ( // ) = Fpath.( // ) in
  let ( / ) = Fpath.( / ) in
  let name = String.uncapitalize_ascii name in
  (* odoc uncapitalises the output filename *)
  let odoc_file = ctx.dirs.odoc_dir // obj_dir / (name ^ ".odoc") in
  let odocl_file = ctx.dirs.odocl_dir // obj_dir / (name ^ ".odocl") in
  let input_copy =
    if stash_input then Some (ctx.dirs.odoc_dir // obj_dir / (name ^ ".cmti"))
    else None
  in
  {
    parent_id = Odoc.Id.of_fpath rel_dir;
    input_file;
    input_copy;
    odoc_file;
    odocl_file;
    kind;
    to_output;
    enable_warnings;
  }

let of_intf ctx (pkg : Packages.t) (lib : Packages.libty)
    (m : Packages.modulety) : intf t =
  let intf = m.m_intf in
  let kind =
    `Intf { hidden = m.m_hidden; hash = intf.mif_hash; deps = intf.mif_deps }
  in
  let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
  make_unit ctx ~name ~kind ~rel_dir:(lib_dir pkg lib)
    ~obj_dir:(lib_obj_dir lib) ~input_file:intf.mif_path
    ~enable_warnings:(selected ctx pkg) ~to_output:(selected ctx pkg)
    ~stash_input:(lib.archive_name = None)

let of_impl ctx (pkg : Packages.t) lib (impl : Packages.impl) : impl t option =
  match impl.mip_src_info with
  | None -> None
  | Some { src_path } ->
      let src_id =
        Fpath.(src_lib_dir pkg lib / filename src_path) |> Odoc.Id.of_fpath
      in
      let name = "impl-" ^ (impl.mip_path |> Fpath.rem_ext |> Fpath.basename) in
      Some
        (make_unit ctx ~name
           ~kind:(`Impl { src_id; src_path })
           ~rel_dir:(lib_dir pkg lib) ~obj_dir:(lib_obj_dir lib)
           ~input_file:impl.mip_path ~enable_warnings:false
           ~to_output:(selected ctx pkg) ~stash_input:false)

let of_lib ctx (pkg : Packages.t) (lib : Packages.libty) : Odoc_unit.lib =
  let units =
    List.concat_map
      (fun (m : Packages.modulety) ->
        let i = of_intf ctx pkg lib m in
        let impl = Option.bind m.m_impl (of_impl ctx pkg lib) in
        (i :> module_unit) :: (Option.to_list impl :> module_unit list))
      lib.modules
  in
  let lib_includes =
    named_dirs_of ctx.known (with_transitive_deps ctx.known lib.lib_name)
  in
  let page =
    if writes_pages ctx pkg then
      Some (Landing_pages.library ~dirs:ctx.dirs ~pkg lib)
    else None
  in
  {
    lib_name = lib.lib_name;
    pkgname = Some pkg.name;
    lib_includes;
    units;
    page;
  }

(* Every library of the run. A library name is a findlib name, so one package
   provides it. Two that claim the same name are a broken switch: keep the
   first, as findlib does, and say so. *)
let libs_of ctx (pkgs : Packages.t list) =
  List.fold_left
    (fun acc (pkg : Packages.t) ->
      List.fold_left
        (fun acc (lib : Packages.libty) ->
          match Util.StringMap.find_opt lib.lib_name acc with
          | Some (other : Odoc_unit.lib) ->
              Logs.warn (fun m ->
                  m "Library '%s' is provided by both '%s' and '%s'"
                    lib.lib_name
                    (Option.value ~default:"" other.pkgname)
                    pkg.name);
              acc
          | None -> Util.StringMap.add lib.lib_name (of_lib ctx pkg lib) acc)
        acc pkg.libraries)
    Util.StringMap.empty pkgs

(* A page of the package's documentation: the parent id follows its path below
   the doc directory, and so does the file. *)
let of_doc ctx (pkg : Packages.t) ~kind ~name ~rel_path ~file ~enable_warnings
    ~to_output =
  let rel_dir = Fpath.(doc_dir pkg // parent rel_path |> normalize) in
  make_unit ctx ~name ~kind ~rel_dir ~obj_dir:(page_obj_dir pkg rel_dir)
    ~input_file:file ~enable_warnings ~to_output ~stash_input:false

let of_mld ctx (pkg : Packages.t) (mld : Packages.mld) : mld t =
  of_doc ctx pkg ~kind:`Mld
    ~name:("page-" ^ (mld.mld_path |> Fpath.rem_ext |> Fpath.basename))
    ~rel_path:mld.mld_rel_path ~file:mld.mld_path
    ~enable_warnings:(selected ctx pkg) ~to_output:(selected ctx pkg)

let of_md ctx (pkg : Packages.t) (md : Packages.md) : md t option =
  if Fpath.has_ext ".md" md.md_path then
    Some
      (of_doc ctx pkg ~kind:`Md
         ~name:("page-" ^ (md.md_path |> Fpath.rem_ext |> Fpath.basename))
         ~rel_path:md.md_rel_path ~file:md.md_path
         ~enable_warnings:(selected ctx pkg) ~to_output:(selected ctx pkg))
  else (
    Logs.debug (fun m ->
        m "Skipping non-markdown doc file %a" Fpath.pp md.md_path);
    None)

let of_asset ctx (pkg : Packages.t) (asset : Packages.asset) : asset t =
  of_doc ctx pkg ~kind:`Asset
    ~name:("asset-" ^ Fpath.basename asset.asset_path)
    ~rel_path:asset.asset_rel_path ~file:asset.asset_path ~enable_warnings:false
    ~to_output:true

(* The landing pages the driver writes for a package: one for the package
   (unless it ships its own index.mld) and one for the sources. The page of a
   library is made with the library, in [of_lib], since it lists the library's
   modules. *)
let landing_pages ctx (pkg : Packages.t) : mld t list =
  if not (writes_pages ctx pkg) then []
  else
    let has_index_page =
      List.exists
        (fun (mld : Packages.mld) ->
          Fpath.equal (Fpath.normalize mld.mld_rel_path) (Fpath.v "index.mld"))
        pkg.mlds
    in
    let has_sources =
      List.exists
        (fun (lib : Packages.libty) ->
          List.exists
            (fun (m : Packages.modulety) ->
              match m.m_impl with
              | Some { mip_src_info = Some _; _ } -> true
              | _ -> false)
            lib.modules)
        pkg.libraries
    in
    (if has_index_page then []
     else [ Landing_pages.package ~dirs:ctx.dirs ~pkg ])
    @ if has_sources then [ Landing_pages.src ~dirs:ctx.dirs ~pkg ] else []

let of_package ctx libs (pkg : Packages.t) : Odoc_unit.pkg =
  let libs =
    List.filter_map
      (fun (lib : Packages.libty) -> Util.StringMap.find_opt lib.lib_name libs)
      pkg.libraries
  in
  let pages =
    (landing_pages ctx pkg :> page list)
    @ (List.map (of_mld ctx pkg) pkg.mlds :> page list)
    @ (List.filter_map (of_md ctx pkg) pkg.other_docs :> page list)
    @ (List.map (of_asset ctx pkg) pkg.assets :> page list)
  in
  {
    pkgname = Some pkg.name;
    scope = scope_of ctx.known pkg;
    index = Some (index_of ~dirs:ctx.dirs pkg);
    libs;
    pages;
    deps =
      List.concat_map
        (fun (l : Odoc_unit.lib) ->
          List.filter_map (fun (n, _) -> ctx.known.lib_pkg n) l.lib_includes)
        libs
      |> List.filter (( <> ) pkg.name)
      |> List.sort_uniq String.compare;
    doc_deps = pkg.doc_deps;
  }

(* The top-level index belongs to no package; its scope is every package and
   library we know of. *)
let toplevel known (pkgs : Packages.t list) (page : mld t) : Odoc_unit.pkg =
  let scope =
    {
      page_roots =
        List.map
          (fun (p : Packages.t) -> (p.name, pkg_pages_dir known p.name))
          pkgs;
      lib_roots = named_dirs_of known known.building;
    }
  in
  {
    pkgname = None;
    scope;
    index = None;
    libs = [];
    pages = [ (page :> page) ];
    deps = [];
    doc_deps = [];
  }

let packages ~installed ~dirs ~remap ~selected ~indices_style
    (pkgs : Packages.t list) : pkg list =
  let known = known ~installed ~odoc_dir:dirs.odoc_dir pkgs in
  let ctx = { known; dirs; remap; selected } in
  let libs = libs_of ctx pkgs in
  let built = List.map (of_package ctx libs) pkgs in
  match indices_style with
  | Normal { toplevel_content = None } ->
      built
      @ [
          toplevel known pkgs
            (Landing_pages.package_list ~dirs ~remap ~selected pkgs);
        ]
  | Normal { toplevel_content = Some content } ->
      let content ppf = Format.fprintf ppf "%s" content in
      let page =
        Landing_pages.make_index ~dirs ~rel_dir:(Fpath.v "./")
          ~obj_dir:(Fpath.v "./") ~enable_warnings:true ~content
      in
      built @ [ toplevel known pkgs page ]
  | Voodoo -> built
