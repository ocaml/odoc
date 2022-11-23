(* Rendering of HTML fragments together with metadata. For embedding the
   generated documentation in existing websites.
*)

module Html = Tyxml.Html

let json_of_breadcrumbs (breadcrumbs : Types.breadcrumb list) : Utils.Json.json
    =
  let breadcrumb (b : Types.breadcrumb) =
    `Object
      [
        ("name", `String b.name);
        ("href", `String b.href);
        ("kind", `String (Odoc_document.Url.Path.string_of_kind b.kind));
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

let make ~config ~preamble ~url ~breadcrumbs ~toc ~uses_katex content children =
  let filename = Link.Path.as_filename ~is_flat:(Config.flat config) url in
  let filename = Fpath.add_ext ".json" filename in
  let htmlpp = Html.pp_elt ~indent:(Config.indent config) () in
  let json_to_string json = Utils.Json.to_string json in
  let content ppf =
    Format.pp_print_string ppf
      (json_to_string
         (`Object
           [
             ("uses_katex", `Bool uses_katex);
             ("breadcrumbs", json_of_breadcrumbs breadcrumbs);
             ("toc", json_of_toc toc);
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
  [ { Odoc_document.Renderer.filename; content; children } ]
