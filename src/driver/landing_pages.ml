open Odoc_unit
open Packages

let fpf = Format.fprintf

let make_index ~dirs ?pkg ~rel_dir ?index ~content () =
  let pages, libs =
    match pkg with
    | None -> ([], [])
    | Some pkg ->
        let lib_args =
          List.map (fun lib -> (lib.lib_name, lib_dir pkg lib)) pkg.libraries
        in
        ([ (pkg.name, doc_dir pkg) ], lib_args)
  in
  let { odoc_dir; odocl_dir; mld_dir; _ } = dirs in
  let input_file = Fpath.(mld_dir // rel_dir / "index.mld") in
  let odoc_file = Fpath.(odoc_dir // rel_dir / "page-index.odoc") in
  let odocl_file = Fpath.(odocl_dir // rel_dir / "page-index.odocl") in
  let parent_id = rel_dir |> Odoc.Id.of_fpath in
  let pkg_args = Pkg_args.v ~pages ~libs ~odoc_dir ~odocl_dir in
  Util.with_out_to input_file (fun oc ->
      fpf (Format.formatter_of_out_channel oc) "%t@?" content)
  |> Result.get_ok;
  {
    output_dir = dirs.odoc_dir;
    pkgname = None;
    pkg_args;
    parent_id;
    input_file;
    odoc_file;
    odocl_file;
    enable_warnings = false;
    to_output = true;
    kind = `Mld;
    index;
  }

let module_list ppf lib =
  let modules = List.filter (fun m -> not m.m_hidden) lib.modules in
  match modules with
  | [] -> fpf ppf "No module."
  | _ :: _ ->
      let modules =
        List.sort (fun m m' -> String.compare m.m_name m'.m_name) modules
      in
      fpf ppf "{!modules:";
      List.iter (fun m -> fpf ppf " %s" m.m_name) modules;
      fpf ppf "}@\n"

let library ~dirs ~pkg ~index lib =
  let content ppf =
    fpf ppf "%@toc_status hidden\n";
    fpf ppf "%@order_category libraries\n";
    fpf ppf "{0 Library [%s]}@\n" lib.lib_name;
    fpf ppf "%a@\n" module_list lib
  in
  let rel_dir = lib_dir pkg lib in
  make_index ~dirs ~rel_dir ~pkg ~index ~content ()

let package ~dirs ~pkg ~index =
  let library_list ppf pkg =
    let print_lib lib =
      fpf ppf "{2 Library %s}@\n%a@\n" lib.lib_name module_list lib
    in
    let libraries =
      List.sort
        (fun lib lib' -> String.compare lib.lib_name lib'.lib_name)
        pkg.libraries
    in
    List.iter print_lib libraries
  in
  let content pkg ppf =
    fpf ppf "{0 %s}@\n@\n@\n" pkg.name;
    List.iter
      (fun { mld_rel_path; _ } ->
        let page = mld_rel_path |> Fpath.rem_ext |> Fpath.to_string in
        fpf ppf "@\n{!/%s/doc/%s}@\n" pkg.name page)
      pkg.mlds;
    if not (List.is_empty pkg.libraries) then
      fpf ppf "{1 API}@\n@\n%a@\n" library_list pkg
  in
  let content = content pkg in
  let rel_dir = doc_dir pkg in
  make_index ~dirs ~rel_dir ~index ~content ~pkg ()

let src ~dirs ~pkg ~index =
  let content ppf =
    fpf ppf "%@order_category source\n";
    fpf ppf
      "{0 Sources}@\n\
       This contains the rendered source for [%s]. Use the sidebar to navigate \
       them."
      pkg.name
  in
  let rel_dir = src_dir pkg in
  make_index ~dirs ~rel_dir ~index ~content ()

let package_list ~dirs ~remap all =
  let content all ppf =
    let sorted_packages =
      all |> List.sort (fun n1 n2 -> String.compare n1.name n2.name)
    in
    fpf ppf "{0 List of all packages}@\n";
    let print_pkg pkg =
      if pkg.selected || not remap then
        fpf ppf "- {{:%s/index.html}%s}@\n" pkg.name pkg.name
    in
    List.iter print_pkg sorted_packages
  in
  let content = content all in
  let rel_dir = Fpath.v "./" in
  make_index ~dirs ~rel_dir ~content ()
