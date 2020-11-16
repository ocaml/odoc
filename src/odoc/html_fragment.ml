open Or_error

let from_mld ~xref_base_uri ~env ~output ~warn_error input =
  (* Internal names, they don't have effect on the output. *)
  let page_name = "__fragment_page__" in
  let id = `RootPage (Odoc_model.Names.PageName.of_string page_name) in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let root =
    let file = Odoc_model.Root.Odoc_file.create_page page_name in
    {Odoc_model.Root.id; file; digest}
  in
  let to_html content =
    (* This is a mess. *)
    let page = Odoc_model.Lang.Page.{ name=id; root; content; digest } in
    let env = Env.build env (`Page page) in
    Odoc_xref2.Link.resolve_page env page
    |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error ~filename:input_s
    >>= fun resolved ->

    let page = Odoc_document.Comment.to_ir resolved.content in
    let html = Odoc_html.Generator.doc ~xref_base_uri page in
    let oc = open_out (Fs.File.to_string output) in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@." (Format.pp_print_list (Tyxml.Html.pp_elt ())) html;
    close_out oc;
    Ok ()
  in
  match Fs.File.read input with
  | Error _ as e -> e
  | Ok str ->
    Odoc_loader.read_string id input_s str
    |> Odoc_model.Error.handle_errors_and_warnings ~warn_error
    >>= function
    |`Docs content -> to_html content
    |`Stop -> to_html [] (* TODO: Error? *)
