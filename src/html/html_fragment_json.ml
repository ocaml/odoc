(* Rendering of HTML fragments together with metadata. For embedding the
   generated documentation in existing websites.
*)

module Html = Tyxml.Html
module Url = Odoc_document.Url

let json_of_breadcrumbs (breadcrumbs : Types.breadcrumb list) : Utils.Json.json
    =
  let breadcrumb (b : Types.breadcrumb) =
    `Object
      [
        ("name", `String b.name);
        ("href", `String b.href);
        ("kind", `String (Url.Path.string_of_kind b.kind));
      ]
  in
  let json_breadcrumbs = breadcrumbs |> List.map breadcrumb in
  `Array json_breadcrumbs

let json_of_toc (toc : Types.toc list) : Utils.Json.json =
  let rec section (s : Types.toc) =
    `Object
      [
        ("title", `String s.title_str);
        ("href", `String s.href);
        ("children", `Array (List.map section s.children));
      ]
  in
  let toc_json_list = toc |> List.map section in
  `Array toc_json_list

let make ~config ~preamble ~url ~breadcrumbs ~toc ~uses_katex ~source_anchor
    content children =
  let filename = Link.Path.as_filename ~is_flat:(Config.flat config) url in
  let filename = Fpath.add_ext ".json" filename in
  let htmlpp = Html.pp_elt ~indent:(Config.indent config) () in
  let json_to_string json = Utils.Json.to_string json in
  let source_anchor =
    match source_anchor with Some url -> `String url | None -> `Null
  in
  let content ppf =
    Format.pp_print_string ppf
      (json_to_string
         (`Object
           [
             ("type", `String "documentation");
             ("uses_katex", `Bool uses_katex);
             ("breadcrumbs", json_of_breadcrumbs breadcrumbs);
             ("toc", json_of_toc toc);
             ("source_anchor", source_anchor);
             ( "preamble",
               `String
                 (String.concat ""
                    (List.map (Format.asprintf "%a" htmlpp) preamble)) );
             ( "content",
               `String
                 (String.concat ""
                    (List.map (Format.asprintf "%a" htmlpp) content)) );
           ]))
  in
  { Odoc_document.Renderer.filename; content; children }

let make_src ~config ~url ~breadcrumbs content =
  let filename = Link.Path.as_filename ~is_flat:(Config.flat config) url in
  let filename = Fpath.add_ext ".json" filename in
  let htmlpp = Html.pp_elt ~indent:(Config.indent config) () in
  let json_to_string json = Utils.Json.to_string json in
  let content ppf =
    Format.pp_print_string ppf
      (json_to_string
         (`Object
           [
             ("type", `String "source");
             ("breadcrumbs", json_of_breadcrumbs breadcrumbs);
             ( "content",
               `String
                 (String.concat ""
                    (List.map (Format.asprintf "%a" htmlpp) content)) );
           ]))
  in
  { Odoc_document.Renderer.filename; content; children = [] }
