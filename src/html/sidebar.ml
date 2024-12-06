open Odoc_utils

let toc_to_json ((url, inline) : Odoc_document.Sidebar.entry) : Json.json =
  let config =
    Config.v ~semantic_uris:true ~indent:true ~flat:false ~open_details:false
      ~as_json:true ~remap:[] ()
  in
  let url, kind =
    match url with
    | None -> (`Null, `Null)
    | Some url ->
        let href = Link.href ~config ~resolve:(Link.Base "") url in
        let kind =
          Format.asprintf "%a" Odoc_document.Url.Anchor.pp_kind url.kind
        in

        (`String href, `String kind)
  in
  let inline =
    let inline = Generator.inline ~config ~xref_base_uri:"" [ inline ] in
    let inline =
      String.concat ""
      @@ List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) inline
    in
    `String inline
  in
  `Object [ ("url", url); ("kind", kind); ("content", inline) ]

let pages_to_json ({ name; pages } : Odoc_document.Sidebar.pages) =
  `Object [ ("name", `String name); ("pages", Tree.to_json toc_to_json pages) ]

let libs_to_json ({ name; units } : Odoc_document.Sidebar.library) =
  `Object
    [
      ("name", `String name);
      ("modules", `Array (List.map (Tree.to_json toc_to_json) units));
    ]

let to_json ({ pages; libraries } : Odoc_document.Sidebar.t) =
  let pages = List.map pages_to_json pages in
  let libraries = List.map libs_to_json libraries in
  `Object [ ("pages", `Array pages); ("libraries", `Array libraries) ]
