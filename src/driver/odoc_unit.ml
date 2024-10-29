module Pkg_args = struct
  type t = {
    compile_dir : Fpath.t;
    link_dir : Fpath.t;
    pages : (string * Fpath.t) list;
    libs : (string * Fpath.t) list;
  }

  let map_rel dir = List.map (fun (a, b) -> (a, Fpath.(dir // b)))

  let compiled_pages v = map_rel v.compile_dir v.pages
  let compiled_libs v = map_rel v.compile_dir v.libs
  let linked_pages v = map_rel v.link_dir v.pages
  let linked_libs v = map_rel v.link_dir v.libs

  let combine v1 v2 =
    if v1.compile_dir <> v2.compile_dir then
      Fmt.invalid_arg "combine: compile_dir differs";
    if v1.link_dir <> v2.link_dir then
      Fmt.invalid_arg "combine: link_dir differs";
    {
      compile_dir = v1.compile_dir;
      link_dir = v1.link_dir;
      pages = v1.pages @ v2.pages;
      libs = v1.libs @ v2.libs;
    }

  let pp fmt x =
    let sfp_pp =
      Fmt.(
        list ~sep:comma (fun fmt (a, b) ->
            Format.fprintf fmt "(%s, %a)" a Fpath.pp b))
    in
    Format.fprintf fmt
      "@[<hov>compile_dir: %a@;link_dir: %a@;pages: [%a]@;libs: [%a]@]" Fpath.pp
      x.compile_dir Fpath.pp x.link_dir sfp_pp x.pages sfp_pp x.libs
end

type index = {
  pkg_args : Pkg_args.t;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
}

let pp_index fmt x =
  Format.fprintf fmt
    "@[<hov>pkg_args: %a@;output_file: %a@;json: %b@;search_dir: %a@]"
    Pkg_args.pp x.pkg_args Fpath.pp x.output_file x.json Fpath.pp x.search_dir

type 'a unit = {
  parent_id : Odoc.Id.t;
  odoc_dir : Fpath.t;
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : Pkg_args.t;
  pkgname : string;
  include_dirs : Fpath.Set.t;
  index : index option;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }

and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]

type asset = [ `Asset ]

type all_kinds = [ impl | intf | mld | asset ]
type t = all_kinds unit

let rec pp_kind : all_kinds Fmt.t =
 fun fmt x ->
  match x with
  | `Intf x -> Format.fprintf fmt "`Intf %a" pp_intf_extra x
  | `Impl x -> Format.fprintf fmt "`Impl %a" pp_impl_extra x
  | `Mld -> Format.fprintf fmt "`Mld"
  | `Asset -> Format.fprintf fmt "`Asset"

and pp_intf_extra fmt x =
  Format.fprintf fmt "@[<hov>hidden: %b@;hash: %s@;deps: [%a]@]" x.hidden x.hash
    Fmt.(list ~sep:comma Fpath.pp)
  @@ List.map (fun x -> x.odoc_file) x.deps

and pp_impl_extra fmt x =
  Format.fprintf fmt "@[<hov>src_id: %s@;src_path: %a@]"
    (Odoc.Id.to_string x.src_id)
    Fpath.pp x.src_path

and pp : all_kinds unit Fmt.t =
 fun fmt x ->
  Format.fprintf fmt
    "@[<hov>parent_id: %s@;\
     odoc_dir: %a@;\
     input_file: %a@;\
     output_dir: %a@;\
     odoc_file: %a@;\
     odocl_file: %a@;\
     pkg_args: %a@;\
     pkgname: %s@;\
     include_dirs: [%a]@;\
     index: %a@;\
     kind:%a@;\
     @]"
    (Odoc.Id.to_string x.parent_id)
    Fpath.pp x.odoc_dir Fpath.pp x.input_file Fpath.pp x.output_dir Fpath.pp
    x.odoc_file Fpath.pp x.odocl_file Pkg_args.pp x.pkg_args x.pkgname
    Fmt.(list ~sep:comma Fpath.pp)
    (Fpath.Set.to_list x.include_dirs)
    (Fmt.option pp_index) x.index pp_kind
    (x.kind :> all_kinds)

let of_packages ~output_dir ~linked_dir ~index_dir ~extra_libs_paths
    (pkgs : Packages.t list) : t list =
  let linked_dir =
    match linked_dir with None -> output_dir | Some dir -> dir
  in
  let index_dir = match index_dir with None -> output_dir | Some dir -> dir in

  let doc_dir pkg = Fpath.(pkg.Packages.pkg_dir / "doc") in

  let lib_dir pkg libname = Fpath.(pkg.Packages.pkg_dir / "lib" / libname) in

  (* [module_of_hash] Maps a hash to the corresponding [Package.t], library name and
     [Packages.modulety]. [lib_dirs] maps a library name to the odoc dir containing its
     odoc files. *)
  let module_of_hash, lib_dirs =
    let open Packages in
    let h = Util.StringMap.empty in
    let lds = extra_libs_paths in
    List.fold_left
      (fun (h, lds) pkg ->
        List.fold_left
          (fun (h, lds) lib ->
            let h' =
              List.fold_left
                (fun h mod_ ->
                  Util.StringMap.add mod_.m_intf.mif_hash (pkg, lib, mod_) h)
                h lib.modules
            in
            let lib_dir = lib_dir pkg lib.lib_name in
            let lds' = Util.StringMap.add lib.lib_name lib_dir lds in
            (h', lds'))
          (h, lds) pkg.libraries)
      (h, lds) pkgs
  in
  let pkg_map =
    Util.StringMap.of_list (List.map (fun pkg -> (pkg.Packages.name, pkg)) pkgs)
  in

  let dash_p pkg = (pkg.Packages.name, doc_dir pkg) in

  let dash_l lib_name =
    match Util.StringMap.find_opt lib_name lib_dirs with
    | Some dir -> [ (lib_name, dir) ]
    | None ->
        Logs.err (fun m -> m "Library %s not found" lib_name);
        []
  in
  (* Given a pkg,  *)
  let base_args pkg lib_deps : Pkg_args.t =
    let own_page = dash_p pkg in
    let own_libs = List.concat_map dash_l (Util.StringSet.to_list lib_deps) in
    {
      pages = [ own_page ];
      libs = own_libs;
      compile_dir = output_dir;
      link_dir = linked_dir;
    }
  in
  let args_of_config config : Pkg_args.t =
    let { Global_config.deps = { packages; libraries } } = config in
    let pages_rel =
      List.filter_map
        (fun pkgname ->
          match Util.StringMap.find_opt pkgname pkg_map with
          | None -> None
          | Some pkg -> Some (dash_p pkg))
        packages
    in
    let libs_rel = List.concat_map dash_l libraries in
    {
      pages = pages_rel;
      libs = libs_rel;
      compile_dir = output_dir;
      link_dir = linked_dir;
    }
  in
  let args_of =
    let cache = Hashtbl.create 10 in
    fun pkg lib_deps : Pkg_args.t ->
      match Hashtbl.find_opt cache (pkg, lib_deps) with
      | Some res -> res
      | None ->
          let result =
            Pkg_args.combine (base_args pkg lib_deps)
              (args_of_config pkg.Packages.config)
          in
          Hashtbl.add cache (pkg, lib_deps) result;
          result
  in

  let index_of pkg =
    let pkg_libs =
      List.map (fun l -> l.Packages.lib_name) pkg.Packages.libraries
      |> Util.StringSet.of_list
    in
    let pkg_args = base_args pkg pkg_libs in
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    { pkg_args; output_file; json = false; search_dir = pkg.pkg_dir }
  in

  let make_unit ~name ~kind ~rel_dir ~input_file ~pkg ~include_dirs ~lib_deps :
      _ unit =
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let pkg_args = args_of pkg lib_deps in
    let odoc_dir = output_dir // rel_dir in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file = odoc_dir / (String.uncapitalize_ascii name ^ ".odoc") in
    (* odoc will uncapitalise the output filename *)
    let odocl_file =
      linked_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odocl")
    in
    {
      output_dir;
      pkgname = pkg.Packages.name;
      pkg_args;
      parent_id;
      odoc_dir;
      input_file;
      odoc_file;
      odocl_file;
      include_dirs;
      kind;
      index = Some (index_of pkg);
    }
  in
  let missing = ref Util.StringSet.empty in

  let rec build_deps deps =
    List.filter_map
      (fun (_name, hash) ->
        match Util.StringMap.find_opt hash module_of_hash with
        | None ->
            missing := Util.StringSet.add hash !missing;
            None
        | Some (pkg, lib, mod_) ->
            let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
            let result =
              of_intf mod_.m_hidden pkg lib.lib_name lib_deps mod_.m_intf
            in
            Some result)
      deps
  and of_intf =
    (* Memoize (using the hash as the key) the creation of interface units, to
       avoid creating them twice *)
    let intf_cache : (string, intf unit) Hashtbl.t = Hashtbl.create 10 in
    fun hidden pkg libname lib_deps (intf : Packages.intf) : intf unit ->
      let do_ () : intf unit =
        let rel_dir = lib_dir pkg libname in
        let include_dirs, kind =
          let deps = build_deps intf.mif_deps in
          let include_dirs =
            List.map (fun u -> u.odoc_dir) deps |> Fpath.Set.of_list
          in
          let kind = `Intf { hidden; hash = intf.mif_hash; deps } in
          (include_dirs, kind)
        in
        let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
        make_unit ~name ~kind ~rel_dir ~input_file:intf.mif_path ~pkg
          ~include_dirs ~lib_deps
      in
      match Hashtbl.find_opt intf_cache intf.mif_hash with
      | Some unit -> unit
      | None ->
          let unit = do_ () in
          Hashtbl.add intf_cache intf.mif_hash unit;
          unit
  in
  let of_impl pkg libname lib_deps (impl : Packages.impl) : impl unit option =
    match impl.mip_src_info with
    | None -> None
    | Some { src_path } ->
        let rel_dir = lib_dir pkg libname in
        let include_dirs =
          let deps = build_deps impl.mip_deps in
          List.map (fun u -> u.odoc_dir) deps |> Fpath.Set.of_list
        in
        let kind =
          let src_name = Fpath.filename src_path in
          let src_id =
            Fpath.(pkg.pkg_dir / "src" / libname / src_name) |> Odoc.Id.of_fpath
          in
          `Impl { src_id; src_path }
        in
        let name =
          impl.mip_path |> Fpath.rem_ext |> Fpath.basename
          |> String.uncapitalize_ascii |> ( ^ ) "impl-"
        in
        let unit =
          make_unit ~name ~kind ~rel_dir ~input_file:impl.mip_path ~pkg
            ~include_dirs ~lib_deps
        in
        Some unit
  in

  let of_module pkg libname lib_deps (m : Packages.modulety) :
      [ impl | intf ] unit list =
    let i :> [ impl | intf ] unit =
      of_intf m.m_hidden pkg libname lib_deps m.m_intf
    in
    let m :> [ impl | intf ] unit list =
      Option.bind m.m_impl (of_impl pkg libname lib_deps) |> Option.to_list
    in
    i :: m
  in
  let of_lib pkg (lib : Packages.libty) : [ impl | intf ] unit list =
    let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
    List.concat_map (of_module pkg lib.lib_name lib_deps) lib.modules
  in
  let of_mld pkg (mld : Packages.mld) : mld unit list =
    let open Fpath in
    let { Packages.mld_path; mld_rel_path } = mld in
    let rel_dir = doc_dir pkg // Fpath.parent mld_rel_path |> Fpath.normalize in
    let include_dirs =
      List.map
        (fun (lib : Packages.libty) -> lib_dir pkg lib.lib_name)
        pkg.libraries
    in
    let include_dirs =
      (output_dir // rel_dir) :: include_dirs |> Fpath.Set.of_list
    in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let unit =
      make_unit ~name ~kind ~rel_dir ~input_file:mld_path ~pkg ~include_dirs
        ~lib_deps:Util.StringSet.empty
    in
    [ unit ]
  in
  let of_asset pkg (asset : Packages.asset) : asset unit list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      doc_dir pkg // Fpath.parent asset_rel_path |> Fpath.normalize
    in
    let include_dirs = Fpath.Set.empty in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~input_file:asset_path ~pkg ~include_dirs
        ~lib_deps:Util.StringSet.empty
    in
    [ unit ]
  in

  let of_package (pkg : Packages.t) : t list =
    let lib_units :> t list list = List.map (of_lib pkg) pkg.libraries in
    let mld_units :> t list list = List.map (of_mld pkg) pkg.mlds in
    let asset_units :> t list list = List.map (of_asset pkg) pkg.assets in
    List.concat (lib_units @ mld_units @ asset_units)
  in
  List.concat_map of_package pkgs
