open Or_error

let link_page ~resolver ~filename page =
  let env = Resolver.build_env_for_page resolver page in
  Odoc_xref2.Link.resolve_page ~filename env page

let content_for_hidden_modules =
  let open Odoc_model in
  let open Lang.Signature in
  let with_loc v =
    let open Location_ in
    {
      location =
        {
          file = "_none_";
          start = { line = 1; column = 0 };
          end_ = { line = 1; column = 0 };
        };
      value = v;
    }
  in
  let sentence =
    [
      `Word "This";
      `Space;
      `Word "module";
      `Space;
      `Word "is";
      `Space;
      `Word "hidden.";
    ]
  in
  [ Comment (`Docs [ with_loc @@ `Paragraph (List.map with_loc sentence) ]) ]

let link_unit ~resolver ~filename m =
  let open Odoc_model in
  let open Lang.Compilation_unit in
  let m =
    if Root.Odoc_file.hidden m.root.file then
      {
        m with
        content =
          Module
            { items = content_for_hidden_modules; compiled = false; doc = [] };
        expansion = None;
      }
    else m
  in
  let env = Resolver.build_link_env_for_unit resolver m in
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
  | Source_tree_content st ->
      Odoc_file.save_source_tree output ~warnings:[] st;
      Ok (`Source_tree st)
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
      (* Remove the shape here so that we only depend upon odoc types
         rather than odoc and ocaml types. This means we don't break
         being able save an odocl file with odoc x.y compiled with one
         version of the compiler and load it in odoc x.y compiled with
         a different version of the compiler, provided the compiler
         itself doesn't break cross-version marshalling! This ability
         is currently being used by voodoo. *)
      let m =
        let open Odoc_model.Lang.Compilation_unit in
        { m with shape_info = None }
      in
      Odoc_file.save_unit output ~warnings m;
      Ok (`Module m)
