open Odoc_model
open Or_error

let resolve_imports resolver imports =
  List.map
    (function
      | Lang.Compilation_unit.Import.Resolved _ as resolved -> resolved
      | Unresolved (name, _) as unresolved -> (
          match Resolver.resolve_import resolver name with
          | Some root -> Resolved (root, Names.ModuleName.make_std name)
          | None -> unresolved))
    imports

let resolve_and_substitute ~resolver ~make_root ~source_id input_file =
  let filename = Fs.File.to_string input_file in
  let impl =
    Odoc_loader.read_impl ~make_root ~filename ~source_id
    |> Error.raise_errors_and_warnings
  in
  let impl = { impl with imports = resolve_imports resolver impl.imports } in
  let env = Resolver.build_compile_env_for_impl resolver impl in
  Odoc_xref2.Compile.compile_impl ~filename env impl |> Error.raise_warnings

let root_of_implementation ~source_id ~module_name ~digest =
  let open Root in
  let result =
    let file = Odoc_file.create_impl module_name in
    let id :> Paths.Identifier.OdocId.t = source_id in
    Ok { id; file; digest }
  in
  result

let compile ~resolver ~output ~warnings_options ~source_path ~source_parent_file
    input =
  ( Odoc_file.load source_parent_file >>= fun parent ->
    let err_not_parent () =
      Error (`Msg "Specified source-parent is not a parent of the source.")
    in
    match parent.Odoc_file.content with
    | Odoc_file.Source_tree_content page -> (
        match page.Lang.SourceTree.name with
        | { Paths.Identifier.iv = `Page _; _ } as parent_id ->
            let id = Paths.Identifier.Mk.source_page (parent_id, source_path) in
            if List.exists (Paths.Identifier.equal id) page.source_children then
              Ok id
            else err_not_parent ()
        | { iv = `LeafPage _; _ } -> err_not_parent ())
    | Unit_content _ | Page_content _ | Impl_content _ ->
        Error (`Msg "Specified source-parent should be a page but is a module.")
  )
  >>= fun source_id ->
  let make_root = root_of_implementation ~source_id in
  let result =
    Error.catch_errors_and_warnings (fun () ->
        resolve_and_substitute ~resolver ~make_root ~source_id input)
  in
  (* Extract warnings to write them into the output file *)
  let _, warnings = Error.unpack_warnings result in
  Error.handle_errors_and_warnings ~warnings_options result >>= fun impl ->
  Odoc_file.save_impl output ~warnings impl;
  Ok ()
