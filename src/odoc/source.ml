open Odoc_utils
open ResultMonad
open Odoc_model

let resolve_and_substitute ~resolver ~make_root ~source_id input_file =
  let filename = Fs.File.to_string input_file in
  let impl =
    Odoc_loader.read_impl ~make_root ~filename ~source_id
    |> Error.raise_errors_and_warnings
  in
  let impl =
    { impl with imports = Compile.resolve_imports resolver impl.imports }
  in
  let env = Resolver.build_compile_env_for_impl resolver impl in
  Odoc_xref2.Compile.compile_impl ~filename env impl |> Error.raise_warnings

let root_of_implementation ~module_name ~digest =
  let open Root in
  let file = Odoc_file.create_impl module_name in
  let id :> Paths.Identifier.OdocId.t =
    Paths.Identifier.Mk.implementation module_name
  in
  Ok { id; file; digest }

let compile ~resolver ~output ~warnings_options ~source_id input =
  let source_id =
    match source_id with
    | None -> Ok None
    | Some source_id ->
        let parent_id, name = Fpath.(split_base (v source_id)) in
        if parent_id = Fpath.v "./" then
          Error (`Msg "Source id cannot be in the root directory")
        else
          let parent =
            match Compile.mk_id Fpath.(to_string (rem_empty_seg parent_id)) with
            | Some s -> Ok s
            | None ->
                Error
                  (`Msg
                     "parent-id cannot be empty when compiling implementations.")
          in
          parent >>= fun parent ->
          let source_id =
            Paths.Identifier.Mk.source_page (parent, Fpath.to_string name)
          in
          Ok (Some source_id)
  in
  source_id >>= fun source_id ->
  let result =
    Error.catch_errors_and_warnings (fun () ->
        resolve_and_substitute ~resolver ~make_root:root_of_implementation
          ~source_id input)
  in
  (* Extract warnings to write them into the output file *)
  let _, warnings = Error.unpack_warnings result in
  Error.handle_errors_and_warnings ~warnings_options result >>= fun impl ->
  Odoc_file.save_impl output ~warnings impl;
  Ok ()
