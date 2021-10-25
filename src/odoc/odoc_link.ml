open Or_error

let link_page ~resolver ~filename page =
  let env = Resolver.build_env_for_page resolver page in
  Odoc_xref2.Link.resolve_page ~filename env page

let link_unit ~resolver ~filename m =
  let open Odoc_model in
  let open Lang.Compilation_unit in
  let m =
    if Root.Odoc_file.hidden m.root.file then
      {
        m with
        content = Module { items = []; compiled = false; doc = [] };
        expansion = None;
      }
    else m
  in
  let env = Resolver.build_env_for_unit resolver ~linking:true m in
  Odoc_xref2.Link.link ~filename env m

(** [~input_warnings] are the warnings stored in the input file *)
let handle_warnings ~input_warnings ~warnings_options ww =
  let _, warnings = Odoc_model.Error.unpack_warnings ww in
  Odoc_model.Error.handle_warnings ~warnings_options ww >>= fun res ->
  Ok (res, input_warnings @ warnings)

(** Read the input file and write to the output file.
    Also return the resulting tree. *)
let from_odoc ~resolver ~warnings_options input output =
  let filename = Fs.File.to_string input in
  Odoc_file.load input >>= fun unit ->
  let input_warnings = unit.Odoc_file.warnings in
  match unit.content with
  | Page_content page ->
      link_page ~resolver ~filename page
      |> handle_warnings ~input_warnings ~warnings_options
      >>= fun (page, warnings) ->
      Odoc_file.save_page output ~warnings page;
      Ok (`Page page)
  | Unit_content m ->
      link_unit ~resolver ~filename m
      |> handle_warnings ~input_warnings ~warnings_options
      >>= fun (m, warnings) ->
      Odoc_file.save_unit output ~warnings m;
      Ok (`Module m)
