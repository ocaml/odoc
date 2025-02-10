open Odoc_unit
open Packages

let fpf = Format.fprintf

let make_index ~dirs ~rel_dir ~libs ~pkgs ~index ~enable_warnings ~content :
    Odoc_unit.mld Odoc_unit.t =
  let { odoc_dir; odocl_dir; mld_dir; _ } = dirs in
  let input_file = Fpath.(mld_dir // rel_dir / "index.mld") in
  let odoc_file = Fpath.(odoc_dir // rel_dir / "page-index.odoc") in
  let odocl_file = Fpath.(odocl_dir // rel_dir / "page-index.odocl") in
  let parent_id = rel_dir |> Odoc.Id.of_fpath in
  let pages =
    List.map (fun pkg -> (pkg.Packages.name, Odoc_unit.doc_dir pkg)) pkgs
  in
  let libs =
    List.map
      (fun (pkg, lib) -> (lib.Packages.lib_name, Odoc_unit.lib_dir pkg lib))
      libs
  in
  let pkg_args = Pkg_args.v ~pages ~libs ~includes:[] ~odoc_dir ~odocl_dir in
  Util.with_out_to input_file (fun oc ->
      fpf (Format.formatter_of_out_channel oc) "%t@?" content)
  |> Result.get_ok;
  {
    output_dir = dirs.odoc_dir;
    pkgname = None;
    pkg_args;
    parent_id;
    input_file;
    input_copy = None;
    odoc_file;
    odocl_file;
    enable_warnings;
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
  let libs = [ (pkg, lib) ] in
  make_index ~dirs ~rel_dir ~libs ~pkgs:[] ~index:(Some index) ~content
    ~enable_warnings:false

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
        fpf ppf "@\n{!/%s/%s}@\n" pkg.name page)
      pkg.mlds;
    if not (List.is_empty pkg.libraries) then
      fpf ppf "{1 API}@\n@\n%a@\n" library_list pkg
  in
  let content = content pkg in
  let rel_dir = doc_dir pkg in
  let libs = List.map (fun lib -> (pkg, lib)) pkg.libraries in
  make_index ~dirs ~rel_dir ~index:(Some index) ~content ~pkgs:[ pkg ] ~libs
    ~enable_warnings:false

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
  make_index ~dirs ~pkgs:[] ~libs:[] ~rel_dir ~index:(Some index) ~content
    ~enable_warnings:true

let package_list ~dirs ~remap all =
  let content all ppf =
    let sorted_packages =
      all |> List.sort (fun n1 n2 -> String.compare n1.name n2.name)
    in
    fpf ppf "{0 List of all packages}@\n";
    let print_pkg pkg =
      if pkg.selected || not remap then
        fpf ppf "- {{!/%s/page-index}%s}@\n" pkg.name pkg.name
    in
    List.iter print_pkg sorted_packages
  in
  let content = content all in
  let rel_dir = Fpath.v "./" in
  make_index ~dirs ~rel_dir ~pkgs:all ~libs:[] ~index:None ~content
    ~enable_warnings:true

let content dir _pkg libs _src subdirs all_libs pfp =
  let is_root = Fpath.to_string dir = "./" in
  fpf pfp "{0 Directory: %a}\n\n" Fpath.pp dir;

  if is_root then (
    fpf pfp "@short_title /\n";
    fpf pfp "@children_order ";
    Fpath.Set.iter
      (fun x ->
        if Fpath.basename x <> "opam_switch" then
          fpf pfp "%s/ " (Fpath.basename x))
      subdirs;
    fpf pfp "opam_switch\n%!")
  else fpf pfp "@short_title %s\n" (Fpath.basename dir);

  if Fpath.Set.cardinal subdirs > 0 then (
    fpf pfp "{1 Subdirectories}\n";
    Fpath.Set.iter
      (fun subdir ->
        fpf pfp "- {{!/%s/%apage-index}%s}\n%!" Monorepo_style.monorepo_pkg_name
          Fpath.pp subdir (Fpath.basename subdir))
      subdirs);

  if (not is_root) && List.length libs > 0 then
    List.iter
      (fun (_, lib) ->
        fpf pfp "{1 Library %s}" lib.Packages.lib_name;
        fpf pfp "%a@\n" module_list lib)
      libs;

  if is_root then (
    fpf pfp "{1 Libraries index}\n";
    List.iter
      (fun lib ->
        fpf pfp "- Library [%s]\n" lib.Packages.lib_name;
        fpf pfp "  %a@\n" module_list lib)
      all_libs)

let make_custom dirs index_of (pkg : Packages.t) :
    Odoc_unit.mld Odoc_unit.t list =
  let pkgs = [ pkg ] in
  let pkg_dirs =
    List.fold_right
      (fun pkg dirs ->
        Fpath.Map.add (Fpath.to_dir_path pkg.Packages.pkg_dir) pkg dirs)
      pkgs Fpath.Map.empty
  in
  let lib_dirs =
    List.fold_right
      (fun pkg dirs ->
        let libs = pkg.libraries in
        List.fold_left
          (fun dirs lib ->
            Fpath.Map.add
              (Fpath.to_dir_path (Odoc_unit.lib_dir pkg lib))
              (pkg, lib) dirs)
          dirs libs)
      pkgs Fpath.Map.empty
  in
  let src_dirs =
    List.fold_right
      (fun pkg dirs ->
        let libs = pkg.libraries in
        let x =
          List.fold_right
            (fun lib dirs ->
              if
                List.exists
                  (fun m ->
                    match m.Packages.m_impl with
                    | Some { mip_src_info = Some _; _ } -> true
                    | _ -> false)
                  lib.modules
              then
                Fpath.Map.add
                  (Fpath.to_dir_path (Odoc_unit.src_lib_dir pkg lib))
                  (pkg, lib) dirs
              else dirs)
            libs dirs
        in
        x)
      pkgs Fpath.Map.empty
  in
  let pkg_src_dirs =
    List.fold_left
      (fun acc pkg ->
        Fpath.Map.add (Odoc_unit.src_dir pkg |> Fpath.to_dir_path) pkg acc)
      Fpath.Map.empty pkgs
  in
  let all_dirs =
    Fpath.Set.union (Fpath.Map.dom pkg_dirs)
      (Fpath.Set.union (Fpath.Map.dom lib_dirs) (Fpath.Map.dom src_dirs))
  in
  let rec all_parents path =
    let parent, _ = Fpath.split_base path in
    if
      Fpath.compare parent (Fpath.v "./") = 0
      || Fpath.compare parent (Fpath.v "/") = 0
    then [ path ]
    else path :: all_parents parent
  in
  let all_dirs =
    Fpath.Set.fold
      (fun p acc ->
        let parents = all_parents p in
        List.fold_right Fpath.Set.add parents acc)
      all_dirs all_dirs
  in

  let all_indexes =
    List.fold_right
      (fun pkg acc ->
        let mlds = pkg.Packages.mlds in
        let indexes =
          List.filter
            (fun mld -> Fpath.basename mld.mld_rel_path = "index.mld")
            mlds
        in
        let index_paths =
          List.map
            (fun mld -> Fpath.(pkg.pkg_dir // mld.mld_rel_path |> parent))
            indexes
          |> Fpath.Set.of_list
        in
        Fpath.Set.union acc index_paths)
      pkgs Fpath.Set.empty
  in

  Fpath.Set.fold
    (fun p acc ->
      if Fpath.Set.mem p all_indexes then (
        Logs.debug (fun m -> m "Skipping predefined index.mld: %a" Fpath.pp p);
        acc)
      else
        let libs =
          let is_root = Fpath.to_string p = "./" in
          Fpath.Map.fold
            (fun p' lib libs -> if p = p' || is_root then lib :: libs else libs)
            lib_dirs []
        in
        let src = Fpath.Map.find_opt p src_dirs in
        let pkg_src = Fpath.Map.find_opt p pkg_src_dirs in
        let subdirs =
          Fpath.Set.filter (fun p' -> Fpath.parent p' = p) all_dirs
        in
        Logs.debug (fun x ->
            x "dir: %a pkg: %a lib: %a src: %a pkg_src: %a subdirs: %a" Fpath.pp
              p Fmt.string pkg.Packages.name (Fmt.Dump.list Fmt.string)
              (List.map (fun (_, p) -> p.Packages.lib_name) libs)
              (Fmt.Dump.option Fmt.string)
              (Option.map (fun (_, p) -> p.Packages.lib_name) src)
              (Fmt.Dump.option Fmt.string)
              (Option.map (fun p -> p.Packages.name) pkg_src)
              (Fmt.Dump.list Fpath.pp)
              (Fpath.Set.elements subdirs));
        let index = Some (index_of pkg) in
        let pkgs = pkgs in
        let all_libs = pkg.libraries in
        Logs.debug (fun m ->
            m "pkgs: %a"
              Fmt.Dump.(list string)
              (List.map (fun p -> p.Packages.name) pkgs));
        let idx =
          make_index ~dirs ~rel_dir:p ~libs ~pkgs
            ~content:(content p pkg libs src subdirs all_libs)
            ~index ~enable_warnings:false
        in
        idx :: acc)
    all_dirs []
