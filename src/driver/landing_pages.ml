open Odoc_unit

let make_index ~dirs ~rel_dir ?index ~content () =
  let { odoc_dir; odocl_dir; mld_dir; _ } = dirs in
  let input_file = Fpath.(mld_dir // rel_dir / "index.mld") in
  let odoc_file = Fpath.(odoc_dir // rel_dir / "page-index.odoc") in
  let odocl_file = Fpath.(odocl_dir // rel_dir / "page-index.odocl") in
  let parent_id = rel_dir |> Odoc.Id.of_fpath in
  Util.with_out_to input_file (fun oc ->
      Format.fprintf (Format.formatter_of_out_channel oc) "%t@?" content)
  |> Result.get_ok;
  {
    output_dir = dirs.odoc_dir;
    pkgname = None;
    pkg_args = { Pkg_args.pages = []; libs = []; odoc_dir; odocl_dir };
    parent_id;
    input_file;
    odoc_file;
    odocl_file;
    enable_warnings = false;
    to_output = true;
    kind = `Mld;
    index;
  }

let library ~dirs ~pkg ~index lib =
  let content ppf =
    Format.fprintf ppf "%@toc_status open\n";
    Format.fprintf ppf "{0 Library [%s]}@\n" lib.Packages.lib_name;
    let print_module m =
      if not m.Packages.m_hidden then
        Format.fprintf ppf "- {!%s}@\n" m.Packages.m_name
    in
    List.iter print_module lib.modules
  in
  let rel_dir = lib_dir pkg lib in
  make_index ~dirs ~rel_dir ~index ~content ()

let package ~dirs ~pkg ~index =
  let content ppf =
    Format.fprintf ppf "{0 %s}@\nUse sidebar to navigate." pkg.Packages.name
  in
  let rel_dir = doc_dir pkg in
  make_index ~dirs ~rel_dir ~index ~content ()

let package_list ~dirs all =
  let content all ppf =
    let sorted_packages =
      all
      |> List.sort (fun n1 n2 ->
             String.compare n1.Packages.name n2.Packages.name)
    in
    Format.fprintf ppf "{0 List of all packages}@\n";
    let print_pkg pkg =
      Format.fprintf ppf "- {{:%s/index.html}%s}@\n" pkg.Packages.name pkg.name
    in
    List.iter print_pkg sorted_packages
  in
  let content = content all in
  let rel_dir = Fpath.v "./" in
  make_index ~dirs ~rel_dir ~content ()
