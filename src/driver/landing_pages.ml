open Packages
open Odoc_unit

let make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir rel_path ~content
    ?(include_dirs = []) ~pkgname ~pkg_args () =
  let input_file = Fpath.(mld_dir // rel_path / "index.mld") in
  let odoc_file = Fpath.(odoc_dir // rel_path / "page-index.odoc") in
  let odocl_file = Fpath.(odocl_dir // rel_path / "page-index.odocl") in
  let () = Util.write_file input_file (String.split_on_char '\n' content) in
  let parent_id = rel_path |> Odoc.Id.of_fpath in
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

module PackageLanding = struct
  let content pkg =
    let title = Format.sprintf "{0 %s}\n" pkg.name in
    let documentation =
      match pkg.mlds with
      | _ :: _ ->
          Format.sprintf
            "{1 Documentation pages}\n\n\
             {{!/%s/doc/index}Documentation for %s}\n"
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

  let page ~odoc_dir ~odocl_dir ~mld_dir ~output_dir ~pkg =
    let content = content pkg in
    let rel_path = Fpath.v pkg.name in
    let pkg_args =
      { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
    in
    make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir rel_path ~content
      ~pkgname:pkg.name ~pkg_args ()
end

module PackageList = struct
  let content all =
    let sorted_packages =
      all |> List.sort (fun n1 n2 -> String.compare n1.name n2.name)
    in
    let title = "{0 List of all packages}\n" in
    let s_of_pkg pkg =
      Format.sprintf "- {{!/__driver/%s/index}%s}" pkg.name pkg.name
    in
    let pkg_ul = sorted_packages |> List.map s_of_pkg |> String.concat "\n" in
    title ^ pkg_ul

  let page ~mld_dir ~odoc_dir ~odocl_dir ~output_dir all =
    let content = content all in
    let rel_path = Fpath.v "./" in
    let pkgname = "__driver" in
    let pkg_args =
      { pages = [ (pkgname, Fpath.(odoc_dir // rel_path)) ]; libs = [] }
    in
    make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir ~content ~pkgname
      ~pkg_args rel_path ()
end

module LibraryLanding = struct
  let content lib =
    let title = Format.sprintf "{0 %s}\n" lib.lib_name in
    let s_of_module m =
      if m.m_hidden then None
      else Some (Format.sprintf "- {!%s}" m.Packages.m_name)
    in
    let modules =
      lib.modules |> List.filter_map s_of_module |> String.concat "\n"
    in
    title ^ modules
  let page ~pkg ~odoc_dir ~odocl_dir ~mld_dir ~output_dir lib =
    let content = content lib in
    let rel_path = Fpath.(v pkg.name / "lib" / lib.lib_name) in
    let pkg_args =
      { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
    in
    let include_dirs = [ Fpath.(odoc_dir // rel_path) ] in
    make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir rel_path ~content
      ~pkgname:pkg.name ~include_dirs ~pkg_args ()
end

module PackageLibLanding = struct
  let content pkg =
    let title = Format.sprintf "{0 %s}\n" pkg.name in
    let s_of_lib (lib : Packages.libty) =
      Format.sprintf "- {{!/%s/%s/index}%s}" pkg.name lib.lib_name lib.lib_name
    in
    let libraries = pkg.libraries |> List.map s_of_lib |> String.concat "\n" in
    title ^ libraries

  let page ~pkg ~odoc_dir ~odocl_dir ~mld_dir ~output_dir =
    let content = content pkg in
    let rel_path = Fpath.(v pkg.name / "lib") in
    let pkg_args =
      { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
    in
    make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir rel_path ~content
      ~pkgname:pkg.name ~pkg_args ()
end

let of_package ~mld_dir ~odoc_dir ~odocl_dir ~output_dir pkg =
  let library_pages =
    List.map
      (LibraryLanding.page ~pkg ~odoc_dir ~odocl_dir ~mld_dir ~output_dir)
      pkg.libraries
  in
  let package_landing_page =
    PackageLanding.page ~odoc_dir ~odocl_dir ~mld_dir ~output_dir ~pkg
  in
  let library_list_page =
    PackageLibLanding.page ~odoc_dir ~odocl_dir ~mld_dir ~output_dir ~pkg
  in
  package_landing_page :: library_list_page :: library_pages

let of_packages ~mld_dir ~odoc_dir ~odocl_dir ~output_dir all =
  PackageList.page ~mld_dir ~odoc_dir ~odocl_dir ~output_dir all
  :: List.concat_map (of_package ~mld_dir ~odoc_dir ~odocl_dir ~output_dir) all
