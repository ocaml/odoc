
let from_odoc ~env ~output:root_dir input =
  let root = Root.read input in
  match root.file with
  | Page page_name ->
    let page = Page.load input in
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Xref.resolve_page (Env.resolver resolve_env) page
    in
    let pkg_name = root.package in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    let content = Html.Documentation.to_html odoctree.content in
    let oc =
      let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".html") in
      open_out (Fs.File.to_string f)
    in
    let fmt = Format.formatter_of_out_channel oc in
    List.iter (Format.fprintf fmt "%a" (Tyxml.Html.pp_elt ())) content;
    close_out oc
  | Compilation_unit _ ->
    Printf.eprintf "ERROR: HTML fragments for compilation units are not \
                    currently supported.\n%!";
    exit 1

