
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


let link_children pkgdir parents self children ppf =
  let page_input ppf name =
    let child_fullname = String.concat ~sep:"." (List.rev (name :: self :: parents)) ^ ".tex" in
    let loc = Fs.File.( to_string @@ create ~directory:pkgdir ~name:child_fullname) in
    Format.fprintf ppf  {|@[<v>\input{%s}@,@]|} loc in
  List.iter ~f:(page_input ppf) children


(* We need to take care of linking children ourselves *)
let traverse ~f t =
  let rec aux parents (node:Renderer.page) =
    let children_names = List.map ~f:(fun (x:Renderer.page) -> x.filename) node.children in
    f ~parents ~children_names node.filename node.content;
    List.iter ~f:(aux (node.filename :: parents)) node.children
  in
  aux [] t


let from_odoc ~env ?(syntax=Renderer.OCaml) ?(with_children=true) ~output:root_dir input =
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page page_name ->
    Page.load input >>= fun page ->
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Odoc_xref2.Link.resolve_page resolve_env page
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    let pkg_name = root.package in
    let pkg_dir = mk_pkg_dir root_dir pkg_name in
    let pages = mk_page ~syntax odoctree in
    Renderer.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []); with_tex_file ~pkg_dir ~page_name content
    );
    Ok ()
  | Compilation_unit {hidden = _; _} ->
    Compilation_unit.load input >>= fun unit ->
    let odoctree =
      let env = Env.build env (`Unit unit) in
      Odoc_xref2.Link.link env unit
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings in
    let pkg_dir = mk_pkg_dir  root_dir root.package in
    let pages = mk_compilation_unit ~syntax odoctree in
    traverse pages ~f:(fun ~parents ~children_names name content ->
      let page_name = String.concat ~sep:"." (List.rev @@ name :: parents) in
      with_tex_file ~pkg_dir ~page_name (fun ppf ->
        content ppf;
        if with_children then link_children pkg_dir parents name children_names ppf
      )
    );
    Ok ()
