(* Odoc_generate *)
open StdLabels
open Or_error
open Odoc_document

let document_of_page ~syntax v =
  match syntax with
  | Renderer.Reason -> Odoc_document.Reason.page v
  | Renderer.OCaml -> Odoc_document.ML.page v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Renderer.Reason -> Odoc_document.Reason.compilation_unit v
  | Renderer.OCaml -> Odoc_document.ML.compilation_unit v

let to_html_tree_page ?theme_uri ~syntax v =
  Odoc_html.Generator.render ?theme_uri @@
  document_of_page ~syntax v

let to_html_tree_compilation_unit ?theme_uri ~syntax v =
  Odoc_html.Generator.render ?theme_uri @@
  document_of_compilation_unit ~syntax v

let from_odocl ?(syntax=Renderer.OCaml) ?theme_uri ~output:root_dir input =
  Root.read input >>= fun root ->
  let pages = 
    match root.file with
    | Page page_name ->
      Format.eprintf "Page name should be: %s.html\n%!" page_name; 
      Page.load input >>= fun odoctree ->
      Ok (to_html_tree_page ?theme_uri ~syntax odoctree, false, page_name ^ ".html")
    | Compilation_unit _ ->
      Compilation_unit.load input >>= fun odoctree ->
      Ok (to_html_tree_compilation_unit ?theme_uri ~syntax odoctree, true, "index.html")
  in
  pages >>= fun (pages, name_is_dir, filename) ->
  let base_dir =
    Fs.Directory.reach_from ~dir:root_dir root.package
  in
  Renderer.traverse pages ~f:(fun ~parents name content ->
    let directory =
      let dir =
        List.fold_right ~f:(fun name dir -> Fs.Directory.reach_from ~dir name)
          parents ~init:base_dir
      in
      if name_is_dir
      then Fs.Directory.reach_from ~dir name
      else dir
    in
    let oc =
      Fs.Directory.mkdir_p directory;
      let f = Fs.File.create ~directory ~name:filename in
      open_out (Fs.File.to_string f)
    in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%t@?" content;
    close_out oc);
Ok ()
