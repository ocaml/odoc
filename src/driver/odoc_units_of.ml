open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

let packages ~dirs ~extra_paths ~remap ~indices_style (pkgs : Packages.t list) :
    any list =
  let { odoc_dir; odocl_dir; index_dir; mld_dir = _ } = dirs in

  let extra_libs_paths = extra_paths.Voodoo.libs in
  let extra_libs_of_pkg = extra_paths.Voodoo.libs_of_pkg in
  let extra_pkg_paths = extra_paths.Voodoo.pkgs in

  let lib_dirs =
    let open Packages in
    let lds = extra_libs_paths in
    List.fold_left
      (fun lds pkg ->
        List.fold_left
          (fun lds lib ->
            let lib_dir = lib_dir pkg lib in
            let lds' = Util.StringMap.add lib.lib_name lib_dir lds in
            lds')
          lds pkg.libraries)
      lds pkgs
  in
  let pkg_paths =
    List.fold_left
      (fun acc pkg -> Util.StringMap.add pkg.Packages.name (doc_dir pkg) acc)
      extra_pkg_paths pkgs
  in

  let libs_of_pkg =
    let libs_of_pkg pkg =
      List.map (fun lib -> lib.Packages.lib_name) pkg.Packages.libraries
    in
    List.fold_left
      (fun acc pkg ->
        Util.StringMap.add pkg.Packages.name (libs_of_pkg pkg) acc)
      extra_libs_of_pkg pkgs
  in

  let dash_p pkgname path = (pkgname, path) in

  let dash_l lib_name =
    match Util.StringMap.find_opt lib_name lib_dirs with
    | Some dir -> [ (lib_name, dir) ]
    | None ->
        Logs.debug (fun m -> m "Library %s not found" lib_name);
        []
  in
  let base_args pkg lib_deps : Pkg_args.t =
    let own_page = dash_p pkg.Packages.name (doc_dir pkg) in
    let includes =
      List.concat_map dash_l (Util.StringSet.to_list lib_deps) |> List.map snd
    in
    let libs =
      List.fold_left
        (fun acc lib -> Util.StringSet.add lib.Packages.lib_name acc)
        lib_deps pkg.Packages.libraries
    in
    let libs = List.concat_map dash_l (Util.StringSet.to_list libs) in
    Pkg_args.v ~pages:[ own_page ] ~libs ~includes ~odoc_dir ~odocl_dir
  in
  let args_of_config config : Pkg_args.t =
    let { Global_config.deps = { packages; libraries } } = config in
    let pages_rel =
      List.filter_map
        (fun pkgname ->
          match Util.StringMap.find_opt pkgname pkg_paths with
          | None ->
              Logs.debug (fun m -> m "Package '%s' not found" pkgname);
              None
          | Some path -> Some (dash_p pkgname path))
        packages
    in
    (* Add all liraries from added packages *)
    let libraries_from_pkgs =
      List.filter_map
        (fun pkgname -> Util.StringMap.find_opt pkgname libs_of_pkg)
        packages
    in
    let libraries = List.concat @@ (libraries :: libraries_from_pkgs) in
    let libs_rel = List.concat_map dash_l libraries in
    Pkg_args.v ~pages:pages_rel ~libs:libs_rel ~includes:[] ~odoc_dir ~odocl_dir
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
    let roots = [ Fpath.( // ) odocl_dir (doc_dir pkg) ] in
    let output_file = Fpath.(index_dir / pkg.name / Odoc.index_filename) in
    let pkg_dir = doc_dir pkg in
    let sidebar =
      let output_file = Fpath.(index_dir / pkg.name / Odoc.sidebar_filename) in
      { output_file; json = false; pkg_dir }
    in
    {
      roots;
      output_file;
      json = false;
      search_dir = doc_dir pkg;
      sidebar = Some sidebar;
    }
  in

  let make_unit ~name ~kind ~rel_dir ~input_file ~pkg ~lib_deps ~enable_warnings
      ~to_output ~stash_input : _ t =
    let to_output = to_output || not remap in
    (* If we haven't got active remapping, we output everything *)
    let ( // ) = Fpath.( // ) in
    let ( / ) = Fpath.( / ) in
    let pkg_args = args_of pkg lib_deps in
    let parent_id = rel_dir |> Odoc.Id.of_fpath in
    let odoc_file =
      odoc_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odoc")
    in
    (* odoc will uncapitalise the output filename *)
    let odocl_file =
      odocl_dir // rel_dir / (String.uncapitalize_ascii name ^ ".odocl")
    in
    let input_copy =
      if stash_input then
        Some (odoc_dir // rel_dir / (String.uncapitalize_ascii name ^ ".cmti"))
      else None
    in
    {
      output_dir = odoc_dir;
      pkgname = Some pkg.Packages.name;
      pkg_args;
      parent_id;
      input_file;
      input_copy;
      odoc_file;
      odocl_file;
      kind;
      to_output;
      enable_warnings;
      index = Some (index_of pkg);
    }
  in

  let of_intf hidden pkg (lib : Packages.libty) lib_deps (intf : Packages.intf)
      : intf t =
    let rel_dir = lib_dir pkg lib in
    let kind =
      let deps = intf.mif_deps in
      let kind = `Intf { hidden; hash = intf.mif_hash; deps } in
      kind
    in
    let name = intf.mif_path |> Fpath.rem_ext |> Fpath.basename in
    let stash_input = lib.archive_name = None in
    make_unit ~name ~kind ~rel_dir ~input_file:intf.mif_path ~pkg ~lib_deps
      ~enable_warnings:pkg.selected ~to_output:pkg.selected ~stash_input
  in
  let of_impl pkg lib lib_deps (impl : Packages.impl) : impl t option =
    match impl.mip_src_info with
    | None -> None
    | Some { src_path } ->
        let rel_dir = lib_dir pkg lib in
        let kind =
          let src_name = Fpath.filename src_path in
          let src_id =
            Fpath.(src_lib_dir pkg lib / src_name) |> Odoc.Id.of_fpath
          in
          `Impl { src_id; src_path }
        in
        let name =
          impl.mip_path |> Fpath.rem_ext |> Fpath.basename
          |> String.uncapitalize_ascii |> ( ^ ) "impl-"
        in
        let unit =
          make_unit ~name ~kind ~rel_dir ~input_file:impl.mip_path ~pkg
            ~lib_deps ~enable_warnings:false ~to_output:pkg.selected
            ~stash_input:false
        in
        Some unit
  in

  let of_module pkg (lib : Packages.libty) lib_deps (m : Packages.modulety) :
      any list =
    let i :> any = of_intf m.m_hidden pkg lib lib_deps m.m_intf in
    let m :> any list =
      Option.bind m.m_impl (of_impl pkg lib lib_deps) |> Option.to_list
    in
    i :: m
  in
  let of_lib pkg (lib : Packages.libty) =
    let lib_deps = Util.StringSet.add lib.lib_name lib.lib_deps in
    let index = index_of pkg in
    let units = List.concat_map (of_module pkg lib lib_deps) lib.modules in
    if remap && not pkg.selected then units
    else
      let landing_page :> any = Landing_pages.library ~dirs ~pkg ~index lib in
      landing_page :: units
  in
  let of_mld pkg (mld : Packages.mld) : mld t list =
    let open Fpath in
    let { Packages.mld_path; mld_rel_path } = mld in
    let rel_dir = doc_dir pkg // Fpath.parent mld_rel_path |> Fpath.normalize in
    let kind = `Mld in
    let name = mld_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-" in
    let lib_deps =
      pkg.libraries
      |> List.map (fun lib -> lib.Packages.lib_name)
      |> Util.StringSet.of_list
    in
    let unit =
      make_unit ~name ~kind ~rel_dir ~input_file:mld_path ~pkg ~lib_deps
        ~enable_warnings:pkg.selected ~to_output:pkg.selected ~stash_input:false
    in
    [ unit ]
  in
  let of_md pkg (md : Packages.md) : md t list =
    let ext = Fpath.get_ext md.md_path in
    match ext with
    | ".md" ->
        let open Fpath in
        let { Packages.md_path; md_rel_path } = md in
        let rel_dir =
          doc_dir pkg // Fpath.parent md_rel_path |> Fpath.normalize
        in
        let kind = `Md in
        let name =
          md_path |> Fpath.rem_ext |> Fpath.basename |> ( ^ ) "page-"
        in
        let lib_deps = Util.StringSet.empty in
        let unit =
          make_unit ~name ~kind ~rel_dir ~input_file:md_path ~pkg ~lib_deps
            ~enable_warnings:pkg.selected ~to_output:pkg.selected
            ~stash_input:false
        in
        [ unit ]
    | _ ->
        Logs.debug (fun m ->
            m "Skipping non-markdown doc file %a" Fpath.pp md.md_path);
        []
  in
  let of_asset pkg (asset : Packages.asset) : asset t list =
    let open Fpath in
    let { Packages.asset_path; asset_rel_path } = asset in
    let rel_dir =
      doc_dir pkg // Fpath.parent asset_rel_path |> Fpath.normalize
    in
    let kind = `Asset in
    let unit =
      let name = asset_path |> Fpath.basename |> ( ^ ) "asset-" in
      make_unit ~name ~kind ~rel_dir ~input_file:asset_path ~pkg
        ~lib_deps:Util.StringSet.empty ~enable_warnings:false ~to_output:true
        ~stash_input:false
    in
    [ unit ]
  in

  let of_package (pkg : Packages.t) : any list =
    let lib_units :> any list list = List.map (of_lib pkg) pkg.libraries in
    let mld_units :> any list list = List.map (of_mld pkg) pkg.mlds in
    let asset_units :> any list list = List.map (of_asset pkg) pkg.assets in
    let md_units :> any list list = List.map (of_md pkg) pkg.other_docs in
    let pkg_index () :> any list =
      let has_index_page =
        List.exists
          (fun mld ->
            Fpath.equal
              (Fpath.normalize mld.Packages.mld_rel_path)
              (Fpath.normalize (Fpath.v "./index.mld")))
          pkg.mlds
      in
      if has_index_page || (remap && not pkg.selected) then []
      else
        let index = index_of pkg in
        [ Landing_pages.package ~dirs ~pkg ~index ]
    in
    let src_index () :> any list =
      if remap && not pkg.selected then []
      else if
        (* Some library has a module which has an implementation which has a source *)
        List.exists
          (fun lib ->
            List.exists
              (fun m ->
                match m.Packages.m_impl with
                | Some { mip_src_info = Some _; _ } -> true
                | _ -> false)
              lib.Packages.modules)
          pkg.libraries
      then
        let index = index_of pkg in
        [ Landing_pages.src ~dirs ~pkg ~index ]
      else []
    in
    let std_units = mld_units @ asset_units @ md_units @ lib_units in
    match indices_style with
    | Automatic when pkg.name = Monorepo_style.monorepo_pkg_name ->
        let others :> any list =
          Landing_pages.make_custom dirs index_of
            (List.find
               (fun p -> p.Packages.name = Monorepo_style.monorepo_pkg_name)
               pkgs)
        in
        others @ List.concat std_units
    | Normal _ | Voodoo | Automatic ->
        List.concat (pkg_index () :: src_index () :: std_units)
  in
  match indices_style with
  | Normal { toplevel_content = None } ->
      let gen_indices :> any = Landing_pages.package_list ~dirs ~remap pkgs in
      gen_indices :: List.concat_map of_package pkgs
  | Normal { toplevel_content = Some content } ->
      let content ppf = Format.fprintf ppf "%s" content in
      let libs =
        List.concat_map
          (fun pkg -> List.map (fun lib -> (pkg, lib)) pkg.Packages.libraries)
          pkgs
      in
      let index :> any =
        Landing_pages.make_index ~dirs
          ~rel_dir:Fpath.(v "./")
          ~libs ~pkgs ~enable_warnings:true ~content ~index:None
      in
      index :: List.concat_map of_package pkgs
  | Voodoo | Automatic -> List.concat_map of_package pkgs
