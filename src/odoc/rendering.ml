open Odoc_document
open Or_error

let document_of_odocl ~syntax input =
  Odoc_file.load input >>= fun unit ->
  match unit.content with
  | Odoc_file.Page_content odoctree ->
      Ok (Renderer.document_of_page ~syntax odoctree, None, None)
  | Unit_content odoctree ->
      Ok
        ( Renderer.document_of_compilation_unit ~syntax odoctree,
          odoctree.impl_source,
          odoctree.intf_source )

let document_of_input ~resolver ~warnings_options ~syntax input =
  let output = Fs.File.(set_ext ".odocl" input) in
  Odoc_link.from_odoc ~resolver ~warnings_options input output >>= function
  | `Page page -> Ok (Renderer.document_of_page ~syntax page, None, None)
  | `Module m ->
      Ok
        ( Renderer.document_of_compilation_unit ~syntax m,
          m.impl_source,
          m.intf_source )

let render_document renderer ~output:root_dir ~extra_suffix ~extra
    (odoctree, impl_source, intf_source) =
  let pages = renderer.Renderer.render extra odoctree in
  let impl_source_page =
    impl_source
    |> Option.map (fun impl_source ->
           renderer.Renderer.render_src extra impl_source odoctree "ml")
    |> Option.value ~default:[]
  in
  let intf_source_page =
    intf_source
    |> Option.map (fun intf_source ->
           renderer.Renderer.render_src extra intf_source odoctree "mli")
    |> Option.value ~default:[]
  in
  Renderer.traverse
    (pages @ impl_source_page @ intf_source_page)
    ~f:(fun filename content ->
      let filename =
        match extra_suffix with
        | Some s -> Fpath.add_ext s filename
        | None -> filename
      in
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc);
  Ok ()

let render_odoc ~resolver ~warnings_options ~syntax ~renderer ~output extra file
    =
  document_of_input ~resolver ~warnings_options ~syntax file
  >>= render_document renderer ~output ~extra_suffix:None ~extra

let generate_odoc ~syntax ~renderer ~output ~extra_suffix extra file =
  document_of_odocl ~syntax file
  >>= render_document renderer ~output ~extra_suffix ~extra

let targets_odoc ~resolver ~warnings_options ~syntax ~renderer ~output:root_dir
    ~extra odoctree =
  let doc =
    if Fpath.get_ext odoctree = ".odoc" then
      document_of_input ~resolver ~warnings_options ~syntax odoctree
    else document_of_odocl ~syntax odoctree
  in
  doc >>= fun (odoctree, _impl_source, _intf_source) ->
  let pages = renderer.Renderer.render extra odoctree in
  Renderer.traverse pages ~f:(fun filename _content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      Format.printf "%a\n" Fpath.pp filename);
  Ok ()
