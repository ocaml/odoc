open Odoc_document
open Or_error
open Odoc_model

let prepare ~extra_suffix ~output_dir filename =
  let filename =
    match extra_suffix with
    | Some s -> Fpath.add_ext s filename
    | None -> filename
  in
  let filename = Fpath.normalize @@ Fs.File.append output_dir filename in
  let directory = Fs.File.dirname filename in
  Fs.Directory.mkdir_p directory;
  filename

let document_of_odocl ~syntax input =
  Odoc_file.load input >>= fun unit ->
  match unit.content with
  | Odoc_file.Page_content odoctree ->
      Ok (Renderer.document_of_page ~syntax odoctree)
  | Unit_content odoctree ->
      Ok (Renderer.document_of_compilation_unit ~syntax odoctree)
  | Impl_content _ ->
      Error
        (`Msg
          "Wrong kind of unit: Expected a page or module unit, got an \
           implementation. Use the dedicated command for implementation.")
  | Asset_content _ ->
      Error
        (`Msg
          "Wrong kind of unit: Expected a page or module unit, got an asset \
           unit. Use the dedicated command for assets.")

let document_of_input ~resolver ~warnings_options ~syntax input =
  let output = Fs.File.(set_ext ".odocl" input) in
  Odoc_link.from_odoc ~resolver ~warnings_options input output >>= function
  | `Page page -> Ok (Renderer.document_of_page ~syntax page)
  | `Module m -> Ok (Renderer.document_of_compilation_unit ~syntax m)
  | `Impl _ ->
      Error
        (`Msg
          "Wrong kind of unit: Expected a page or module unit, got an \
           implementation. Use the dedicated command for implementation.")
  | `Asset _ ->
      Error
        (`Msg
          "Wrong kind of unit: Expected a page or module unit, got an asset \
           unit. Use the dedicated command for assets.")

let render_document renderer ~sidebar ~output:root_dir ~extra_suffix ~extra doc
    =
  let pages = renderer.Renderer.render extra sidebar doc in
  Renderer.traverse pages ~f:(fun filename content ->
      let filename = prepare ~extra_suffix ~output_dir:root_dir filename in
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc)

let render_odoc ~resolver ~warnings_options ~syntax ~renderer ~output extra file
    =
  let extra_suffix = None in
  document_of_input ~resolver ~warnings_options ~syntax file >>= fun doc ->
  render_document renderer ~sidebar:None ~output ~extra_suffix ~extra doc;
  Ok ()

let generate_odoc ~syntax ~warnings_options:_ ~renderer ~output ~extra_suffix
    ~sidebar extra file =
  (match sidebar with
  | None -> Ok None
  | Some x ->
      Odoc_file.load_index x >>= fun index ->
      Ok (Some (Odoc_document.Sidebar.of_lang index)))
  >>= fun sidebar ->
  document_of_odocl ~syntax file >>= fun doc ->
  render_document renderer ~output ~sidebar ~extra_suffix ~extra doc;
  Ok ()

let documents_of_implementation ~warnings_options:_ ~syntax impl source_file =
  match impl.Lang.Implementation.id with
  | Some _ -> (
      match Fs.File.read source_file with
      | Error (`Msg msg) ->
          Error (`Msg (Format.sprintf "Couldn't load source file: %s" msg))
      | Ok source_code ->
          let syntax_info =
            Syntax_highlighter.syntax_highlighting_locs source_code
          in
          let rendered =
            Odoc_document.Renderer.documents_of_implementation ~syntax impl
              syntax_info source_code
          in
          Ok rendered)
  | None ->
      Error (`Msg "The implementation unit was not compiled with --source-id.")

let generate_source_odoc ~syntax ~warnings_options ~renderer ~output
    ~source_file ~extra_suffix extra file =
  Odoc_file.load file >>= fun unit ->
  match unit.content with
  | Odoc_file.Impl_content impl ->
      documents_of_implementation ~warnings_options ~syntax impl source_file
      >>= fun docs ->
      List.iter
        (render_document renderer ~output ~sidebar:None ~extra_suffix ~extra)
        docs;
      Ok ()
  | Page_content _ | Unit_content _ | Asset_content _ ->
      Error (`Msg "Expected an implementation unit")

let generate_asset_odoc ~warnings_options:_ ~renderer ~output ~asset_file
    ~extra_suffix extra file =
  Odoc_file.load file >>= fun unit ->
  match unit.content with
  | Odoc_file.Asset_content unit ->
      let url = Odoc_document.Url.Path.from_identifier unit.name in
      let filename = renderer.Renderer.filepath extra url in
      let dst = prepare ~extra_suffix ~output_dir:output filename in
      Fs.File.copy ~src:asset_file ~dst
  | Page_content _ | Unit_content _ | Impl_content _ ->
      Error (`Msg "Expected an asset unit")

let targets_odoc ~resolver ~warnings_options ~syntax ~renderer ~output:root_dir
    ~extra odoctree =
  let doc =
    if Fpath.get_ext odoctree = ".odoc" then
      document_of_input ~resolver ~warnings_options ~syntax odoctree
    else document_of_odocl ~syntax odoctree
  in
  doc >>= fun doc ->
  let pages = renderer.Renderer.render extra None doc in
  Renderer.traverse pages ~f:(fun filename _content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      Format.printf "%a\n" Fpath.pp filename);
  Ok ()

let targets_source_odoc ~syntax ~warnings_options ~renderer ~output:root_dir
    ~extra ~source_file odoctree =
  Odoc_file.load odoctree >>= fun unit ->
  match unit.content with
  | Odoc_file.Impl_content impl ->
      documents_of_implementation ~warnings_options ~syntax impl source_file
      >>= fun docs ->
      List.iter
        (fun doc ->
          let pages = renderer.Renderer.render extra None doc in
          Renderer.traverse pages ~f:(fun filename _content ->
              let filename =
                Fpath.normalize @@ Fs.File.append root_dir filename
              in
              Format.printf "%a\n" Fpath.pp filename))
        docs;
      Ok ()
  | Page_content _ | Unit_content _ | Asset_content _ ->
      Error (`Msg "Expected an implementation unit")
