open Odoc_unit

let copy ~src ~dst =
  Cmd_outputs.v ~output:dst ~desc:"Copying the interface"
    Bos.Cmd.(v "cp" % Fpath.to_string src % Fpath.to_string dst)

let compile_module (lib : lib) (u : module_unit) =
  match u.kind with
  | `Intf _ ->
      Odoc.compile ~output_file:u.odoc_file ~input_file:u.input_file
        ~libs:lib.lib_includes ~warnings_tag:lib.pkgname ~parent_id:u.parent_id
        ~ignore_output:(not u.enable_warnings)
      :: Option.to_list
           (Option.map (fun dst -> copy ~src:u.input_file ~dst) u.input_copy)
  | `Impl { src_id; _ } ->
      [
        Odoc.compile_impl ~output_file:u.odoc_file ~input_file:u.input_file
          ~libs:lib.lib_includes ~parent_id:u.parent_id ~source_id:src_id;
      ]

let compile_page (p : page) =
  match p.kind with
  | `Mld ->
      Odoc.compile ~output_file:p.odoc_file ~input_file:p.input_file ~libs:[]
        ~warnings_tag:None ~parent_id:p.parent_id
        ~ignore_output:(not p.enable_warnings)
  | `Md ->
      Odoc.compile_md ~output_file:p.odoc_file ~input_file:p.input_file
        ~parent_id:p.parent_id
  | `Asset ->
      Odoc.compile_asset ~output_file:p.odoc_file ~parent_id:p.parent_id
        ~name:(Fpath.filename p.input_file)

(* Libraries sharing an object directory share an odoc directory, so their -L
   roots overlap; --custom-layout tells odoc that is intended. *)
let link ~warnings_tags (pkg : pkg) (u : any) =
  Odoc.link ~input_file:u.odoc_file ~output_file:u.odocl_file
    ~libs:pkg.scope.lib_roots ~docs:pkg.scope.page_roots
    ~ignore_output:(not u.enable_warnings) ~warnings_tags
    ?current_package:pkg.pkgname ()

let index ~html_dir ~file_list (index : index) =
  let db = Fpath.(html_dir // Sherlodoc.db_js_file index.html_dir) in
  ( Odoc.compile_index ~json:false ~output_file:index.index_file ~file_list
      ~simplified:false ~wrap:false (),
    [
      Odoc.sidebar_generate ~output_file:index.sidebar_file ~json:false
        index.index_file ();
      Odoc.sidebar_generate
        ~output_file:Fpath.(html_dir // index.html_dir / "sidebar.json")
        ~json:true index.index_file ();
      Sherlodoc.index ~inputs:[ index.index_file ] ~dst:db;
    ] )

let generate ~html_dir ?remap_file ~generate_json (pkg : pkg) (u : any) =
  let output_dir = Fpath.to_string html_dir in
  let home_breadcrumb = "Package index" in
  (* A package's pages point at its sidebar and search database. *)
  let sidebar, search_uris =
    match pkg.index with
    | None -> (None, None)
    | Some index ->
        ( Some index.sidebar_file,
          Some [ Sherlodoc.db_js_file index.html_dir; Sherlodoc.js_file ] )
  in
  let input_file = u.odocl_file in
  (* The JSON output, for ocaml.org, is not remapped. *)
  let html_and_json f =
    f ~as_json:false ~remap:remap_file
    :: (if generate_json then [ f ~as_json:true ~remap:None ] else [])
  in
  if not (is_output u) then []
  else
    match u.kind with
    | `Impl { src_path; _ } ->
        html_and_json (fun ~as_json ~remap:_ ->
            Odoc.html_generate_source ?search_uris ?sidebar ~output_dir
              ~input_file ~source:src_path ~as_json ~home_breadcrumb ())
    | `Asset ->
        [
          Odoc.html_generate_asset ~output_dir ~input_file:u.odoc_file
            ~asset_path:u.input_file ~home_breadcrumb ();
        ]
    | `Intf _ | `Mld | `Md ->
        html_and_json (fun ~as_json ~remap ->
            Odoc.html_generate ?search_uris ?sidebar ?remap ~output_dir
              ~input_file ~as_json ~home_breadcrumb ())
