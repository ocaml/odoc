open Packages

let pkg_landing_page_content pkg =
  let title = Format.sprintf "{0 %s}\n" pkg.name in
  let documentation =
    match pkg.mlds with
    | _ :: _ ->
        Format.sprintf
          "{1 Documentation pages}\n\n{{!/%s/doc/index}Documentation for %s}\n"
          pkg.name pkg.name
    | [] -> ""
  in
  let libraries =
    match pkg.libraries with
    | [] -> ""
    | _ :: _ ->
        Format.sprintf "{1 Libraries}\n\n{{!/%s/lib/index}Libraries for %s}\n"
          pkg.name pkg.name
  in
  title ^ documentation ^ libraries

let library_landing_page_content lib =
  let title = Format.sprintf "{0 %s}\n" lib.lib_name in
  let s_of_module m =
    if m.m_hidden then None
    else Some (Format.sprintf "- {!%s}" m.Packages.m_name)
  in
  let modules =
    lib.modules |> List.filter_map s_of_module |> String.concat "\n"
  in
  title ^ modules

let libraries_landing_page_content pkg =
  let title = Format.sprintf "{0 %s}\n" pkg.name in
  let s_of_lib (lib : Packages.libty) =
    Format.sprintf "- {{!/%s/%s/index}%s}" pkg.name lib.lib_name lib.lib_name
  in
  let libraries = pkg.libraries |> List.map s_of_lib |> String.concat "\n" in
  title ^ libraries

let list_packages_content all =
  let sorted_packages =
    all |> List.sort (fun n1 n2 -> String.compare n1.name n2.name)
  in
  let title = "{0 List of all packages}\n" in
  let s_of_pkg pkg = Format.sprintf "- {{!/%s/index}%s}" pkg.name pkg.name in
  let pkg_ul = sorted_packages |> List.map s_of_pkg |> String.concat "\n" in
  title ^ pkg_ul

let write_file file content = Bos.OS.File.write file content |> Result.get_ok

let of_package ~mld_dir ~odoc_dir ~odocl_dir ~output_dir pkg =
  let make_unit rel_path ~content ?(include_dirs = []) ~pkgname ~pkg_args () =
    let input_file = Fpath.(mld_dir // rel_path / "index.mld") in
    let odoc_file = Fpath.(odoc_dir // rel_path / "page-index.odoc") in
    let odocl_file = Fpath.(odocl_dir // rel_path / "page-index.odocl") in
    let () = write_file input_file content in
    let parent_id = rel_path |> Odoc.id_of_fpath in
    let open Odoc_unit in
    {
      parent_id;
      odoc_dir;
      input_file;
      output_dir;
      odoc_file;
      odocl_file;
      pkg_args;
      pkgname;
      include_dirs;
      index = None;
      kind = `Mld;
    }
  in
  let library_list_page =
    let open Odoc_unit in
    let content = libraries_landing_page_content pkg in
    let rel_path = Fpath.(v pkg.name / "lib") in
    let pkg_args =
      { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
    in
    make_unit rel_path ~content ~pkgname:pkg.name ~pkg_args ()
  in
  let library_landing_pages =
    let do_ lib =
      let open Odoc_unit in
      let content = library_landing_page_content lib in
      let rel_path = Fpath.(v pkg.name / "lib" / lib.lib_name) in
      let pkg_args =
        { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
      in
      let include_dirs = [ Fpath.(odoc_dir // rel_path) ] in
      make_unit rel_path ~content ~pkgname:pkg.name ~include_dirs ~pkg_args ()
    in
    List.map do_ pkg.libraries
  in
  let package_landing_page =
    let open Odoc_unit in
    let content = pkg_landing_page_content pkg in
    let rel_path = Fpath.v pkg.name in
    let pkg_args =
      { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
    in
    make_unit rel_path ~content ~pkgname:pkg.name ~pkg_args ()
  in
  package_landing_page :: library_list_page :: library_landing_pages

let of_packages ~mld_dir ~odoc_dir ~odocl_dir ~output_dir all =
  let content = list_packages_content all in
  let rel_path = Fpath.v "a" in
  let input_file = Fpath.(mld_dir // rel_path / "index.mld") in
  let () = write_file input_file content in
  let open Odoc_unit in
  let parent_id = rel_path |> Odoc.id_of_fpath in
  let pkgname = "__driver" in
  let pkg_args =
    {
      pages =
        (pkgname, Fpath.(odoc_dir // rel_path))
        :: List.map (fun pkg -> (pkg.name, Fpath.(odoc_dir / pkg.name))) all;
      libs = [];
    }
  in
  {
    parent_id;
    odoc_dir;
    input_file;
    output_dir;
    pkg_args;
    pkgname;
    odoc_file = Fpath.(odoc_dir // rel_path / "page-index.odoc");
    odocl_file = Fpath.(odocl_dir // rel_path / "page-index.odocl");
    include_dirs = [];
    index = None;
    kind = `Mld;
  }
  :: List.concat_map (of_package ~mld_dir ~odoc_dir ~odocl_dir ~output_dir) all

(* let compile_list_packages odoc_dir all : compiled = *)
(*   let sorted_packages = *)
(*     all |> Util.StringMap.to_list *)
(*     |> List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) *)
(*   in *)
(*   let title = "{0 List of all packages}\n" in *)
(*   let s_of_pkg (name, _) = Format.sprintf "- {{!%s/index}%s}" name name in *)
(*   let pkg_ul = sorted_packages |> List.map s_of_pkg |> String.concat "\n" in *)
(*   let content = title ^ pkg_ul in *)
(*   let input_file = Fpath.( / ) odoc_dir "index.mld" in *)
(*   let () = Bos.OS.File.write input_file content |> Result.get_ok in *)
(*   Odoc.compile ~output_dir:odoc_dir ~input_file ~includes:Fpath.Set.empty *)
(*     ~parent_id:(Odoc.id_of_fpath (Fpath.v "./")); *)
(*   Atomic.incr Stats.stats.compiled_mlds; *)
(*   { *)
(*     m = Mld; *)
(*     odoc_output_dir = odoc_dir; *)
(*     odoc_file = Fpath.(odoc_dir / "page-index.odoc"); *)
(*     odocl_file = Fpath.(odoc_dir / "page-index.odocl"); *)
(*     include_dirs = Fpath.Set.empty; *)
(*     impl = None; *)
(*     pkg_args = { docs = [ ("_driver_pkg", odoc_dir) ]; libs = [] }; *)
(*     pkgname = { p_name = "_driver_pkg"; p_dir = Fpath.v "./" }; *)
(*   } *)

(* let compile_landing_pages odoc_dir pkg : compiled list = *)
(*   let pkgname = pkg.Packages.pkgname in *)
(*   let driver_page ~odoc_file ~odocl_file ?(include_dirs = Fpath.Set.empty) () = *)
(*     let pkg_args = *)
(*       { *)
(*         docs = [ (pkgname.p_name, Fpath.( / ) odoc_dir pkgname.p_name) ]; *)
(*         libs = []; *)
(*       } *)
(*     in *)
(*     { *)
(*       m = Mld; *)
(*       odoc_output_dir = odoc_dir; *)
(*       odoc_file; *)
(*       odocl_file; *)
(*       include_dirs; *)
(*       impl = None; *)
(*       pkg_args; *)
(*       pkgname; *)
(*     } *)
(*   in *)
(*   let title = Format.sprintf "{0 %s}\n" in *)
(*   let compile ~content ~input_file ?(include_dirs = Fpath.Set.empty) ~parent_id *)
(*       () = *)
(*     let () = Bos.OS.File.write input_file content |> Result.get_ok in *)
(*     Odoc.compile ~output_dir:odoc_dir ~input_file ~includes:include_dirs *)
(*       ~parent_id; *)
(*     Atomic.incr Stats.stats.compiled_mlds; *)
(*     ( Fpath.(odoc_dir // Odoc.fpath_of_id parent_id / "page-index.odoc"), *)
(*       Fpath.(odoc_dir // Odoc.fpath_of_id parent_id / "page-index.odocl") ) *)
(*   in *)

(*   let library_landing_page pkgname (lib : Packages.libty) : compiled = *)
(*     let libname = lib.lib_name in *)
(*     let parent_id = *)
(*       Fpath.(v pkgname.Packages.p_name / "lib" / libname) |> Odoc.id_of_fpath *)
(*     in *)
(*     let input_file = *)
(*       Fpath.(odoc_dir // Odoc.fpath_of_id parent_id / "index.mld") *)
(*     in *)
(*     let s_of_module m = Format.sprintf "- {!%s}" m.Packages.m_name in *)
(*     let modules = lib.modules |> List.map s_of_module |> String.concat "\n" in *)
(*     let content = title libname ^ modules in *)
(*     let include_dirs = *)
(*       Fpath.(Set.empty |> Set.add (odoc_dir // Odoc.fpath_of_id parent_id)) *)
(*     in *)
(*     let odoc_file, odocl_file = *)
(*       compile ~content ~input_file ~include_dirs ~parent_id () *)
(*     in *)
(*     driver_page ~odoc_file ~odocl_file ~include_dirs () *)
(*   in *)

(*   let libraries_landing_page pkg : compiled list = *)
(*     let pkgname = pkg.Packages.pkgname in *)
(*     let parent_id = Fpath.(v pkgname.p_name / "lib") |> Odoc.id_of_fpath in *)
(*     let input_file = *)
(*       Fpath.(odoc_dir // Odoc.fpath_of_id parent_id / "index.mld") *)
(*     in *)
(*     let s_of_lib (lib : Packages.libty) = *)
(*       Format.sprintf "- {{!%s/index}%s}" lib.lib_name lib.lib_name *)
(*     in *)
(*     let libraries = pkg.libraries |> List.map s_of_lib |> String.concat "\n" in *)
(*     let content = title pkgname.p_name ^ libraries in *)
(*     let odoc_file, odocl_file = compile ~content ~input_file ~parent_id () in *)
(*     driver_page ~odoc_file ~odocl_file () *)
(*     :: List.map (library_landing_page pkgname) pkg.libraries *)
(*   in *)

(*   let package_landing_page = *)
(*     let input_file = Fpath.(odoc_dir // v pkgname.p_name / "index.mld") in *)
(*     let documentation = *)
(*       match pkg.mlds with *)
(*       | _ :: _ -> *)
(*           Format.sprintf *)
(*             "{1 Documentation pages}\n\n{{!doc/index}Documentation for %s}" *)
(*             pkgname.p_name *)
(*       | [] -> "" *)
(*     in *)
(*     let libraries = *)
(*       match pkg.libraries with *)
(*       | [] -> "" *)
(*       | _ :: _ -> *)
(*           Format.sprintf "{1 Libraries}\n\n{{!lib/index}Libraries for %s}" *)
(*             pkgname.p_name *)
(*     in *)
(*     let content = title pkgname.p_name ^ documentation ^ libraries in *)
(*     let parent_id = Odoc.id_of_fpath (Fpath.v pkgname.p_name) in *)
(*     let odoc_file, odocl_file = compile ~content ~input_file ~parent_id () in *)
(*     driver_page ~odoc_file ~odocl_file () *)
(*   in *)
(*   package_landing_page :: libraries_landing_page pkg *)
