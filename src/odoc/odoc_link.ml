open Odoc_utils
open ResultMonad

let link_page ~resolver ~filename page =
  let env = Resolver.build_env_for_page resolver page in
  Odoc_xref2.Link.resolve_page ~filename env page

let link_impl ~resolver ~filename impl =
  let env = Resolver.build_link_env_for_impl resolver impl in
  Odoc_xref2.Link.resolve_impl ~filename env impl

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
  [
    Comment
      (`Docs
         {
           elements = [ with_loc @@ `Paragraph (List.map with_loc sentence) ];
           warnings_tag = None;
         });
  ]

let link_unit ~resolver ~filename ~warnings_tags m =
  let open Odoc_model in
  let open Lang.Compilation_unit in
  let m =
    if Root.Odoc_file.hidden m.root.file then
      {
        m with
        content =
          Module
            {
              items = content_for_hidden_modules;
              compiled = false;
              removed = [];
              doc = { elements = []; warnings_tag = None };
            };
        expansion = None;
      }
    else m
  in
  let env = Resolver.build_link_env_for_unit resolver m in
  let env = Odoc_xref2.Env.set_warnings_tags env warnings_tags in
  Odoc_xref2.Link.link ~filename env m

(** [~input_warnings] are the warnings stored in the input file *)
let handle_warnings ~input_warnings ~warnings_options ww =
  let _, warnings = Odoc_model.Error.unpack_warnings ww in
  Odoc_model.Error.handle_warnings ~warnings_options ww >>= fun res ->
  Ok (res, input_warnings @ warnings)

(** Read the input file and write to the output file. Also return the resulting
    tree. *)
let from_odoc ~resolver ~warnings_options ~warnings_tags input output =
  let filename = Fs.File.to_string input in
  Odoc_file.load input >>= fun unit ->
  let input_warnings = unit.Odoc_file.warnings in
  match unit.content with
  | Impl_content impl ->
      link_impl ~resolver ~filename impl
      |> handle_warnings ~input_warnings ~warnings_options
      >>= fun (impl, warnings) ->
      (* Remove the shape here so that we only depend upon odoc types
         rather than odoc and ocaml types. This means we don't break
         being able save an odocl file with odoc x.y compiled with one
         version of the compiler and load it in odoc x.y compiled with
         a different version of the compiler, provided the compiler
         itself doesn't break cross-version marshalling! This ability
         is currently being used by voodoo. *)
      let impl = { impl with shape_info = None } in
      Odoc_file.save_impl output ~warnings impl;
      Ok (`Impl impl)
  | Page_content page ->
      link_page ~resolver ~filename page
      |> handle_warnings ~input_warnings ~warnings_options
      >>= fun (page, warnings) ->
      Odoc_file.save_page output ~warnings page;
      Ok (`Page page)
  | Unit_content m ->
      link_unit ~resolver ~filename ~warnings_tags m
      |> handle_warnings ~input_warnings ~warnings_options
      >>= fun (m, warnings) ->
      Odoc_file.save_unit output ~warnings m;
      Ok (`Module m)
  | Asset_content a ->
      Odoc_file.save_asset output ~warnings:[] a;
      Ok (`Asset a)
