open Odoc_document
open Or_error
open Odoc_model

let documents_of_unit ~warnings_options ~syntax ~renderer ~extra unit =
  Error.catch_warnings (fun () ->
      renderer.Renderer.extra_documents extra (CU unit))
  |> Error.handle_warnings ~warnings_options
  >>= fun extra_docs ->
  Ok (Renderer.document_of_compilation_unit ~syntax unit :: extra_docs)

let documents_of_page ~warnings_options ~syntax ~renderer ~extra page =
  Error.catch_warnings (fun () ->
      renderer.Renderer.extra_documents extra (Page page))
  |> Error.handle_warnings ~warnings_options
  >>= fun extra_docs -> Ok (Renderer.document_of_page ~syntax page :: extra_docs)

let documents_of_odocl ~warnings_options ~renderer ~extra ~syntax input =
  Odoc_file.load input >>= fun unit ->
  match unit.content with
  | Odoc_file.Page_content odoctree ->
      documents_of_page ~warnings_options ~syntax ~renderer ~extra odoctree
  | Impl_content _impl ->
      (* documents_of_implementation ~warnings_options ~syntax impl source *)
      Error
        (`Msg
          "Wrong kind of unit: Expected a page or module unit, got an \
           implementation. Use the dedicated command for implementation.")
  | Unit_content odoctree ->
      documents_of_unit ~warnings_options ~syntax ~renderer ~extra odoctree
  | Asset_content _ -> Ok [] (* TODO *)

let documents_of_input ~renderer ~extra ~resolver ~warnings_options ~syntax
    input =
  let output = Fs.File.(set_ext ".odocl" input) in
  Odoc_link.from_odoc ~resolver ~warnings_options input output >>= function
  | `Page page -> Ok [ Renderer.document_of_page ~syntax page ]
  | `Impl _impl ->
      Error
        (`Msg
          "Wrong kind of unit: Expected a page or module unit, got an \
           implementation. Use the dedicated command for implementation.")
  | `Module m -> documents_of_unit ~warnings_options ~syntax ~renderer ~extra m
  | `Asset _ -> Ok [] (* TODO *)

let render_document renderer ~sidebar ~output:root_dir ~extra_suffix ~extra doc
    =
  let url =
    match doc with
    | Odoc_document.Types.Document.Page { url; _ } -> url
    | Source_page { url; _ } -> url
    | Asset { url; _ } -> url
  in
  let sidebar =
    Odoc_utils.Option.map
      (fun sb -> Odoc_document.Sidebar.to_block sb url)
      sidebar
  in
  let pages = renderer.Renderer.render extra sidebar doc in
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
  List.iter
    (render_document renderer ~sidebar:None ~output ~extra_suffix ~extra)
    docs;
  Ok ()

let generate_odoc ~syntax ~warnings_options ~renderer ~output ~extra_suffix
    ~sidebar extra file =
  (match sidebar with
  | None -> Ok None
  | Some x ->
      Odoc_file.load_index x >>= fun (sidebar, _) ->
      Ok (Some (Odoc_document.Sidebar.of_lang sidebar)))
  >>= fun sidebar ->
  documents_of_odocl ~warnings_options ~renderer ~extra ~syntax file
  >>= fun docs ->
  List.iter
    (render_document renderer ~output ~sidebar ~extra_suffix ~extra)
    docs;
  Ok ()

let documents_of_implementation ~warnings_options:_ ~syntax impl source =
  match (source, impl.Lang.Implementation.id) with
  | Some source_file, Some _ -> (
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
  | _, None ->
      Error (`Msg "The implementation unit was not compiled with --source-id.")
  | None, _ ->
      Error
        (`Msg
          "--source should be passed when generating documents for an \
           implementation.")

let generate_impl_odoc ~syntax ~warnings_options ~renderer ~output ~source_file
    ~extra_suffix extra file =
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
      let pages = renderer.Renderer.render extra None doc in
      Renderer.traverse pages ~f:(fun filename _content ->
          let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
          Format.printf "%a\n" Fpath.pp filename))
    docs;
  Ok ()
