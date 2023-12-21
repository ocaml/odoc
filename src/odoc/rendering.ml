open Odoc_document
open Or_error

module Source = struct
  type t = File of Fpath.t | Root of Fpath.t

  let pp fmt = function
    | File f -> Format.fprintf fmt "File: %a" Fpath.pp f
    | Root f -> Format.fprintf fmt "File: %a" Fpath.pp f

  let to_string f = Format.asprintf "%a" pp f
end

type source = Source.t

let check_empty_source_arg source filename =
  if source <> None then
    Odoc_model.Error.raise_warning
    @@ Odoc_model.Error.filename_only
         "--source and --source-root only have an effect when generating from \
          an implementation"
         filename

let documents_of_unit ~warnings_options ~syntax ~source ~renderer ~extra
    ~filename unit =
  Odoc_model.Error.catch_warnings (fun () ->
      check_empty_source_arg source filename;
      renderer.Renderer.extra_documents ~syntax extra (CU unit))
  |> Odoc_model.Error.handle_warnings ~warnings_options
  >>= fun extra_docs ->
  Ok (Renderer.document_of_compilation_unit ~syntax unit :: extra_docs)

let documents_of_page ~warnings_options ~syntax ~source ~renderer ~extra
    ~filename page =
  Odoc_model.Error.catch_warnings (fun () ->
      check_empty_source_arg source filename;
      renderer.Renderer.extra_documents ~syntax extra (Page page))
  |> Odoc_model.Error.handle_warnings ~warnings_options
  >>= fun extra_docs -> Ok (Renderer.document_of_page ~syntax page :: extra_docs)

let documents_of_implementation ~warnings_options:_ ~syntax impl source =
  match source with
  | Some source -> (
      let source_file =
        match source with
        | Source.File f -> f
        | Root f ->
            let open Odoc_model.Paths.Identifier in
            let rec get_path_dir : SourceDir.t -> Fpath.t = function
              | { iv = `SourceDir (d, f); _ } -> Fpath.(get_path_dir d / f)
              | { iv = `Page _; _ } -> f
            in
            let get_path : SourcePage.t -> Fpath.t = function
              | { iv = `SourcePage (d, f); _ } -> Fpath.(get_path_dir d / f)
            in
            get_path impl.Odoc_model.Lang.Implementation.id
      in
      match Fs.File.read source_file with
      | Error (`Msg msg) ->
          Error (`Msg (Format.sprintf "Couldn't load source file: %s" msg))
      | Ok source_code ->
          let syntax_info =
            Syntax_highlighter.syntax_highlighting_locs source_code
          in
          Ok
            [
              Odoc_document.Renderer.documents_of_implementation ~syntax impl
                syntax_info source_code;
            ])
  | None ->
      Error
        (`Msg
          "--source or --source-root should be passed when generating \
           documents for an implementation.")

let documents_of_odocl ~warnings_options ~renderer ~extra ~source ~syntax input
    =
  Odoc_file.load input >>= fun unit ->
  let filename = Fpath.to_string input in
  match unit.content with
  | Odoc_file.Page_content odoctree ->
      documents_of_page ~warnings_options ~syntax ~source ~renderer ~extra
        ~filename odoctree
  | Source_tree_content srctree ->
      Ok (Renderer.documents_of_source_tree ~syntax srctree)
  | Impl_content impl ->
      documents_of_implementation ~warnings_options ~syntax impl source
  | Unit_content odoctree ->
      documents_of_unit ~warnings_options ~source ~syntax ~renderer ~extra
        ~filename odoctree

let documents_of_input ~renderer ~extra ~resolver ~warnings_options ~syntax
    input =
  let output = Fs.File.(set_ext ".odocl" input) in
  Odoc_link.from_odoc ~resolver ~warnings_options input output >>= function
  | `Source_tree st -> Ok (Renderer.documents_of_source_tree ~syntax st)
  | `Page page -> Ok [ Renderer.document_of_page ~syntax page ]
  | `Impl impl -> Ok [ Renderer.documents_of_implementation ~syntax impl [] "" ]
  | `Module m ->
      documents_of_unit ~warnings_options ~source:None ~filename:"" ~syntax
        ~renderer ~extra m

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
    ~source extra file =
  documents_of_odocl ~warnings_options ~renderer ~source ~extra ~syntax file
  >>= fun docs ->
  List.iter (render_document renderer ~output ~extra_suffix ~extra) docs;
  Ok ()

let targets_odoc ~resolver ~warnings_options ~syntax ~renderer ~output:root_dir
    ~extra ~source odoctree =
  let docs =
    if Fpath.get_ext odoctree = ".odoc" then
      documents_of_input ~renderer ~extra ~resolver ~warnings_options ~syntax
        odoctree
    else
      documents_of_odocl ~warnings_options ~renderer ~extra ~syntax ~source
        odoctree
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
