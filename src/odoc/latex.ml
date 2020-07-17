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

let mk_page ~syntax ~with_children v =
  Odoc_latex.Generator.render ~with_children @@
  document_of_page ~syntax v

let mk_compilation_unit ~syntax ~with_children v =
  Odoc_latex.Generator.render ~with_children @@
  document_of_compilation_unit ~syntax v


let with_tex_file ~pkg_dir ~page_name f =
  let oc =
    let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".tex") in
    open_out (Fs.File.to_string f)
  in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%t@?" f;
  close_out oc

let mk_pkg_dir root_dir pkg_name =
  let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
  Fs.Directory.mkdir_p pkg_dir;
  pkg_dir

let from_odoc ~env ?(syntax=Renderer.OCaml) ?(with_children=true) ~output:root_dir input =
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page _ ->
    Page.load input >>= fun page ->
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Odoc_xref2.Link.resolve_page resolve_env page
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    let pages = mk_page ~syntax ~with_children odoctree in
    Renderer.traverse pages ~f:(fun filename content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()
  | Compilation_unit {hidden = _; _} ->
    Compilation_unit.load input >>= fun unit ->
    let odoctree =
      let env = Env.build env (`Unit unit) in
      Odoc_xref2.Link.link env unit
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings in
    let pages = mk_compilation_unit ~syntax ~with_children odoctree in
    Renderer.traverse pages ~f:(fun filename content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()
