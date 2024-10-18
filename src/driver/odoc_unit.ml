type pkg_args = {
  pages : (string * Fpath.t) list;
  libs : (string * Fpath.t) list;
}

type index = {
  pkg_args : pkg_args;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
}

type 'a unit = {
  parent_id : Odoc.Id.t;
  odoc_dir : Fpath.t;
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : pkg_args;
  pkgname : string;
  include_dirs : Fpath.t list;
  index : index option;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]

type asset = [ `Asset ]

type t = [ impl | intf | mld | asset ] unit

let of_packages ~output_dir ~linked_dir ~index_dir (pkgs : Packages.t list) :
    t list =
  let linked_dir =
    match linked_dir with None -> output_dir | Some dir -> dir
  in
  let index_dir = match index_dir with None -> output_dir | Some dir -> dir in

  (* Maps a hash to the corresponding [Package.t], library name and
     [Packages.modulety]. *)
  let module_of_hash =
    let open Packages in
    let h = Util.StringMap.empty in
    List.fold_left
      (fun h pkg ->
        List.fold_left
          (fun h lib ->
            List.fold_left
              (fun h mod_ ->
                Util.StringMap.add mod_.m_intf.mif_hash
                  (pkg, lib.lib_name, mod_) h)
              h lib.modules)
          h pkg.libraries)
      h pkgs
  in
  let pkg_map =
    Util.StringMap.of_list (List.map (fun pkg -> (pkg.Packages.name, pkg)) pkgs)
  in

  let lib_map =
    Util.StringMap.of_list
      (List.concat_map
         (fun pkg ->
           List.map
             (fun lib -> (lib.Packages.lib_name, (pkg, lib)))
             pkg.Packages.libraries)
         pkgs)
  in
  let doc_dir pkg = Fpath.(pkg.Packages.pkg_dir / "doc") in
  let lib_dir pkg libname = Fpath.(pkg.Packages.pkg_dir / "lib" / libname) in
  let make_absolute = Fpath.( // ) output_dir in

  let dash_p pkg = (pkg.Packages.name, doc_dir pkg |> make_absolute) in
  let dash_l pkg lib =
    (lib.Packages.lib_name, lib_dir pkg lib.lib_name |> make_absolute)
  in
  (* Given a pkg,  *)
  let pkg_args_of_pkg pkg : pkg_args =
    let own_page = dash_p pkg in
    let own_libs = List.map (dash_l pkg) pkg.libraries in
    { pages = [ own_page ]; libs = own_libs }
  in
  let pkg_args_of_config config : pkg_args =
    let { Global_config.deps = { packages; libraries } } = config in
    let pages =
      List.filter_map
        (fun pkgname ->
          match Util.StringMap.find_opt pkgname pkg_map with
          | None -> None
          | Some pkg -> Some (dash_p pkg))
        packages
    in
    let libs =
      List.filter_map
        (fun libname ->
          match Util.StringMap.find_opt libname lib_map with
          | None -> None
          | Some (pkg, lib) -> Some (dash_l pkg lib))
        libraries
    in
    { pages; libs }
  in
  let pkg_args =
    let cache = Hashtbl.create 10 in
    fun pkg : pkg_args ->
      match Hashtbl.find_opt cache pkg with
      | Some res -> res
      | None ->
          let { pages = own_page; libs = own_libs } = pkg_args_of_pkg pkg in
          let { pages = config_pages; libs = config_libs } =
            pkg_args_of_config pkg.Packages.config
          in
          { pages = own_page @ config_pages; libs = own_libs @ config_libs }
  in

  let index_of pkg =
    let pkg_args = pkg_args_of_pkg pkg in
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    { pkg_args; output_file; json = false; search_dir = pkg.pkg_dir }
  in

  let make_unit ~name ~kind ~rel_dir ~input_file ~pkg ~include_dirs : _ unit =
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let odoc_dir = output_dir // rel_dir in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file = odoc_dir / (name ^ ".odoc") in
    let odocl_file = linked_dir // rel_dir / (name ^ ".odocl") in
    {
      output_dir;
      pkgname = pkg.Packages.name;
      pkg_args = pkg_args pkg;
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

  let rec build_deps deps =
    List.filter_map
      (fun (_name, hash) ->
        match Util.StringMap.find_opt hash module_of_hash with
        | None -> None
        | Some (pkg, lib, mod_) ->
            let result = of_intf mod_.m_hidden pkg lib mod_.m_intf in
            Some result)
      deps
  and of_intf =
    (* Memoize (using the hash as the key) the creation of interface units, to
       avoid creating them twice *)
    let intf_cache : (string, intf unit) Hashtbl.t = Hashtbl.create 10 in

    fun hidden pkg libname (intf : Packages.intf) : intf unit ->
      let do_ () : intf unit =
        let rel_dir = lib_dir pkg libname in
        let include_dirs, kind =
          let deps = build_deps intf.mif_deps in
          let include_dirs = List.map (fun u -> u.odoc_dir) deps in
          let kind = `Intf { hidden; hash = intf.mif_hash; deps } in
          (include_dirs, kind)
        in
        let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
        make_unit ~name ~kind ~rel_dir ~input_file:intf.mif_path ~pkg
          ~include_dirs
      in
      match Hashtbl.find_opt intf_cache intf.mif_hash with
      | Some unit -> unit
      | None ->
          let result = do_ () in
          Hashtbl.add intf_cache intf.mif_hash result;
          result
  in

  let of_impl pkg libname (impl : Packages.impl) : impl unit option =
    let _ =
      match impl.mip_src_info with
      | None -> None
      | Some { src_path } ->
          let rel_dir = lib_dir pkg libname in
          let include_dirs =
            let deps = build_deps impl.mip_deps in
            List.map (fun u -> u.odoc_dir) deps
          in
          let kind =
            let src_name = Fpath.filename src_path in
            let src_id =
              Fpath.(pkg.pkg_dir / "src" / libname / src_name)
              |> Odoc.Id.of_fpath
            in
            `Impl { src_id; src_path }
          in
          let name =
            impl.mip_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "impl-"
          in
          let unit =
            make_unit ~name ~kind ~rel_dir ~input_file:impl.mip_path ~pkg
              ~include_dirs
          in
          Some unit
    in
    None
  in

  let of_module pkg libname (m : Packages.modulety) : [ impl | intf ] unit list
      =
    let i :> [ impl | intf ] unit = of_intf m.m_hidden pkg libname m.m_intf in
    let m :> [ impl | intf ] unit list =
      Option.bind m.m_impl (of_impl pkg libname) |> Option.to_list
    in
    i :: m
  in
  let of_lib pkg (lib : Packages.libty) : [ impl | intf ] unit list =
    List.concat_map (of_module pkg lib.lib_name) lib.modules
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
    let include_dirs = (output_dir // rel_dir) :: include_dirs in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let unit =
      make_unit ~name ~kind ~rel_dir ~input_file:mld_path ~pkg ~include_dirs
    in
    [ unit ]
  in
  let of_asset pkg (asset : Packages.asset) : asset unit list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      doc_dir pkg // Fpath.parent asset_rel_path |> Fpath.normalize
    in
    let include_dirs = [] in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~input_file:asset_path ~pkg ~include_dirs
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
