(* Rendering of HTML fragments together with metadata. For embedding the
   generated documentation in existing websites.
*)
open Odoc_utils

module Html = Tyxml.Html
module Url = Odoc_document.Url

let json_of_html config h =
  let htmlpp = Html.pp_elt ~indent:(Config.indent config) () in
  String.concat ~sep:"" (List.map (Format.asprintf "%a" htmlpp) h)

let json_of_breadcrumbs config (breadcrumbs : Types.breadcrumbs) : Json.json =
  let breadcrumb (b : Types.breadcrumb) =
    `Object
      [
        ("name", `String (json_of_html config b.name));
        ("href", match b.href with None -> `Null | Some href -> `String href);
        ("kind", `String (Url.Path.string_of_kind b.kind));
      ]
  in
  let json_breadcrumbs =
    breadcrumbs.parents @ [ breadcrumbs.current ] |> List.map breadcrumb
  in
  `Array json_breadcrumbs

let json_of_toc (toc : Types.toc list) : Json.json =
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

let json_of_sidebar config sidebar =
  match sidebar with
  | None -> `Null
  | Some sidebar -> `String (json_of_html config sidebar)

let make ~config ~preamble ~url ~breadcrumbs ~toc ~uses_katex ~source_anchor
    ~header content children =
  let filename = Link.Path.as_filename ~config url in
  let filename = Fpath.add_ext ".json" filename in
  let json_to_string json = Json.to_string json in
  let source_anchor =
    match source_anchor with Some url -> `String url | None -> `Null
  in
  let content ppf =
    Format.pp_print_string ppf
      (json_to_string
         (`Object
            [
              ("header", `String (json_of_html config header));
              ("type", `String "documentation");
              ("uses_katex", `Bool uses_katex);
              ("breadcrumbs", json_of_breadcrumbs config breadcrumbs);
              ("toc", json_of_toc toc);
              ("source_anchor", source_anchor);
              ("preamble", `String (json_of_html config preamble));
              ("content", `String (json_of_html config content));
            ]))
  in
  { Odoc_document.Renderer.filename; content; children; path = url }

let make_src ~config ~url ~breadcrumbs ~sidebar ~header content =
  let filename = Link.Path.as_filename ~config url in
  let filename = Fpath.add_ext ".json" filename in
  let htmlpp = Html.pp_elt ~indent:(Config.indent config) () in
  let json_to_string json = Json.to_string json in
  let global_toc = json_of_sidebar config sidebar in
  let content ppf =
    Format.pp_print_string ppf
      (json_to_string
         (`Object
            [
              ("type", `String "source");
              ("breadcrumbs", json_of_breadcrumbs config breadcrumbs);
              ("global_toc", global_toc);
              ("header", `String (json_of_html config header));
              ( "content",
                `String
                  (String.concat ~sep:""
                     (List.map (Format.asprintf "%a" htmlpp) content)) );
            ]))
  in
  { Odoc_document.Renderer.filename; content; children = []; path = url }
