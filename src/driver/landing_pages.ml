open Packages
open Odoc_unit

type unit_of_html =
  | Html of { path : Fpath.t; content : string }
  | Unit of [ `Mld ] Odoc_unit.unit

let split_units_and_html li :
    (Fpath.t * string) list * [> `Mld ] Odoc_unit.unit list =
  List.partition_map
    (function
      | Html { path; content } -> Left (path, content)
      | Unit unit -> Right (unit :> [< `Mld ] Odoc_unit.unit))
    li
let fpf = Format.fprintf

let make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir rel_path ~content
    ?(include_dirs = []) ~pkgname ~pkg_args () =
  let input_file = Fpath.(mld_dir // rel_path / "index.mld") in
  let odoc_file = Fpath.(odoc_dir // rel_path / "page-index.odoc") in
  let odocl_file = Fpath.(odocl_dir // rel_path / "page-index.odocl") in
  Util.with_out_to input_file (fun oc ->
      fpf (Format.formatter_of_out_channel oc) "%t@?" content)
  |> Result.get_ok;
  let parent_id = rel_path |> Odoc.Id.of_fpath in
  Unit
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

module PackageLibLanding = struct
  let page ~pkg =
    let package_index = Fpath.(v ".." / "index.html") in
    let path = Fpath.(v pkg.name / "lib" / "index.html") in
    let content =
      Format.asprintf
        {|<head><meta http-equiv="refresh" content="1; URL=%a"></head>|}
        Fpath.pp package_index
    in
    Html { path; content }
end

module PackageLanding = struct
  let module_list ppf lib =
    let modules = List.filter (fun m -> not m.m_hidden) lib.modules in
    match modules with
    | [] -> fpf ppf " with no toplevel module."
    | _ :: _ ->
        let modules =
          List.sort (fun m m' -> String.compare m.m_name m'.m_name) modules
        in
        fpf ppf "     {!modules:";
        List.iter (fun m -> fpf ppf " %s" m.m_name) modules;
        fpf ppf "     }@\n"

  let library_list ppf pkg =
    let print_lib (lib : Packages.libty) =
      fpf ppf "{2 Library %s}@\n%a@\n" lib.lib_name module_list lib
    in
    let libraries =
      List.sort
        (fun lib lib' -> String.compare lib.lib_name lib'.lib_name)
        pkg.libraries
    in
    List.iter print_lib libraries

  let content pkg ppf =
    List.iter
      (fun { mld_rel_path; _ } ->
        let page = mld_rel_path |> Fpath.rem_ext |> Fpath.to_string in
        fpf ppf "@\n{!/%s/doc/%s}@\n" pkg.name page)
      pkg.mlds;
    if not (List.is_empty pkg.libraries) then
      fpf ppf "{1 API}@\n@\n%a@\n" library_list pkg

  let include_dirs ~odoc_dir pkg =
    List.map
      (fun lib -> Fpath.(odoc_dir // pkg.pkg_dir / "lib" / lib.lib_name))
      pkg.Packages.libraries

  let page_exists pkg page =
    List.exists
      (fun Packages.{ mld_path = _; mld_rel_path } -> page = mld_rel_path)
      pkg.mlds

  let pkg_has_index pkg = page_exists pkg Fpath.(v "index.mld")

  let pkg_redirect pkg =
    let package_index = Fpath.(v "doc" / "index.html") in
    let path = Fpath.(v pkg.name / "index.html") in
    let content =
      Format.asprintf
        {|<head><meta http-equiv="refresh" content="1; URL=%a"></head>|}
        Fpath.pp package_index
    in
    Html { path; content }

  let page ~odoc_dir ~odocl_dir ~mld_dir ~output_dir ~pkg =
    if pkg_has_index pkg then pkg_redirect pkg
    else
      let content = content pkg in
      let rel_path = Fpath.v pkg.name in
      let pkg_args =
        { pages = [ (pkg.name, Fpath.( // ) odoc_dir rel_path) ]; libs = [] }
      in
      let include_dirs = include_dirs ~odoc_dir pkg in
      make_unit ~odoc_dir ~odocl_dir ~mld_dir ~output_dir rel_path ~content
        ~pkgname:pkg.name ~pkg_args ~include_dirs ()
end

module PackageList = struct
  let content all ppf =
    let sorted_packages =
      all |> List.sort (fun n1 n2 -> String.compare n1.name n2.name)
    in
    fpf ppf "{0 List of all packages}@\n";
    let print_pkg pkg =
      fpf ppf "- {{!/__driver/%s/index}%s}@\n" pkg.name pkg.name
    in
    List.iter print_pkg sorted_packages

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
  let content lib ppf =
    fpf ppf "{0 %s}@\n" lib.lib_name;
    let print_module m =
      if not m.m_hidden then fpf ppf "- {!%s}@\n" m.Packages.m_name
    in
    List.iter print_module lib.modules

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

let of_package ~mld_dir ~odoc_dir ~odocl_dir ~output_dir pkg =
  let library_pages =
    List.map
      (LibraryLanding.page ~pkg ~odoc_dir ~odocl_dir ~mld_dir ~output_dir)
      pkg.libraries
  in
  let package_landing_page =
    PackageLanding.page ~odoc_dir ~odocl_dir ~mld_dir ~output_dir ~pkg
  in
  let library_list_page = PackageLibLanding.page ~pkg in
  package_landing_page :: library_list_page :: library_pages

let of_packages ~mld_dir ~odoc_dir ~odocl_dir ~output_dir all :
    (Fpath.t * string) list * [> `Mld ] Odoc_unit.unit list =
  split_units_and_html
    (PackageList.page ~mld_dir ~odoc_dir ~odocl_dir ~output_dir all
    :: List.concat_map
         (of_package ~mld_dir ~odoc_dir ~odocl_dir ~output_dir)
         all)
