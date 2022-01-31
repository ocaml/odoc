open Result

let resolve url_to_string directories reference =
  let resolver =
    Resolver.create ~important_digests:false ~directories ~open_modules:[]
  in
  let reference =
    let open Odoc_model in
    let warnings_options = { Error.warn_error = true; print_warnings = true } in
    Semantics.parse_reference reference
    |> Error.handle_errors_and_warnings ~warnings_options
  in
  match reference with
  | Error e -> Error e
  | Ok reference -> (
      let environment = Resolver.build_env_for_reference resolver in
      let resolved_reference =
        Odoc_xref2.Ref_tools.resolve_reference environment reference
        |> Odoc_model.Error.raise_warnings
      in
      match resolved_reference with
      | Error e ->
          let error =
            Format.asprintf "%a"
              Odoc_xref2.Errors.Tools_error.pp_reference_lookup_error e
          in
          Error (`Msg error)
      | Ok resolved_reference -> (
          let identifier =
            Odoc_model.Paths.Reference.Resolved.identifier resolved_reference
          in
          let url =
            Odoc_document.Url.from_identifier ~stop_before:false identifier
          in
          match url with
          | Error e -> Error (`Msg (Odoc_document.Url.Error.to_string e))
          | Ok url ->
              let href = url_to_string url in
              print_endline href;
              Ok ()))

let reference_to_url_html base =
  let url_to_string url =
    let href s = Odoc_html.Link.(href ~resolve:(Base s) url) in
    match base with
    | None -> href ""
    | Some base -> (
        match String.rindex base '/' = String.length base - 1 with
        | true -> href base
        | false -> href (base ^ "/"))
  in
  resolve url_to_string

let reference_to_url_latex =
  let url_to_string url = Odoc_latex.Generator.Link.label url in
  resolve url_to_string
