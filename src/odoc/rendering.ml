open Odoc_document
open Or_error

let documents_of_unit ~warnings_options ~syntax ~renderer ~extra unit =
  Odoc_model.Error.catch_warnings (fun () ->
      renderer.Renderer.extra_documents ~syntax extra (CU unit))
  |> Odoc_model.Error.handle_warnings ~warnings_options
  >>= fun extra_docs ->
  Ok (Renderer.document_of_compilation_unit ~syntax unit :: extra_docs)

let documents_of_page ~warnings_options ~syntax ~renderer ~extra page =
  Odoc_model.Error.catch_warnings (fun () ->
      renderer.Renderer.extra_documents ~syntax extra (Page page))
  |> Odoc_model.Error.handle_warnings ~warnings_options
  >>= fun extra_docs -> Ok (Renderer.document_of_page ~syntax page :: extra_docs)

let documents_of_odocl ~warnings_options ~renderer ~extra ~syntax input =
  Odoc_file.load input >>= fun unit ->
  match unit.content with
  | Odoc_file.Page_content odoctree ->
      documents_of_page ~warnings_options ~syntax ~renderer ~extra odoctree
  | Source_tree_content srctree ->
      Ok (Renderer.documents_of_source_tree ~syntax srctree)
  | Unit_content odoctree ->
      documents_of_unit ~warnings_options ~syntax ~renderer ~extra odoctree

let documents_of_input ~renderer ~extra ~resolver ~warnings_options ~syntax
    input =
  let output = Fs.File.(set_ext ".odocl" input) in
  Odoc_link.from_odoc ~resolver ~warnings_options input output >>= function
  | `Source_tree st -> Ok (Renderer.documents_of_source_tree ~syntax st)
  | `Page page -> Ok [ Renderer.document_of_page ~syntax page ]
  | `Module m -> documents_of_unit ~warnings_options ~syntax ~renderer ~extra m

let render_document renderer ~output:root_dir ~extra_suffix ~extra doc =
  let pages = renderer.Renderer.render extra doc in
  Renderer.traverse pages ~f:(fun filename content ->
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
      close_out oc)

let render_odoc ~resolver ~warnings_options ~syntax ~renderer ~output extra file
    =
  let extra_suffix = None in
  documents_of_input ~renderer ~extra ~resolver ~warnings_options ~syntax file
  >>= fun docs ->
  List.iter (render_document renderer ~output ~extra_suffix ~extra) docs;
  Ok ()

let generate_odoc ~syntax ~warnings_options ~renderer ~output ~extra_suffix
    extra file =
  documents_of_odocl ~warnings_options ~renderer ~extra ~syntax file
  >>= fun docs ->
  List.iter (render_document renderer ~output ~extra_suffix ~extra) docs;
  Ok ()

let targets_odoc ~resolver ~warnings_options ~syntax ~renderer ~output:root_dir
    ~extra odoctree =
  let docs =
    if Fpath.get_ext odoctree = ".odoc" then
      documents_of_input ~renderer ~extra ~resolver ~warnings_options ~syntax
        odoctree
    else documents_of_odocl ~warnings_options ~renderer ~extra ~syntax odoctree
  in
  docs >>= fun docs ->
  List.iter
    (fun doc ->
      let pages = renderer.Renderer.render extra doc in
      Renderer.traverse pages ~f:(fun filename _content ->
          let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
          Format.printf "%a\n" Fpath.pp filename))
    docs;
  Ok ()
