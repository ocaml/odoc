open Odoc_utils

let toc_to_json
    ({ url; valid_link; content = inline; _ } : Odoc_document.Sidebar.entry) :
    Json.json =
  let config =
    Config.v ~semantic_uris:true ~indent:true ~flat:false ~open_details:false
      ~as_json:true ~remap:[] ()
  in
  let url, kind =
    match valid_link with
    | false -> (`Null, `Null)
    | true ->
        let href = Link.href ~config ~resolve:(Link.Base "") url in
        let kind =
          Format.asprintf "%a" Odoc_document.Url.Anchor.pp_kind url.kind
        in

        (`String href, `String kind)
  in
  let inline =
    let inline = Generator.inline ~config ~xref_base_uri:"" inline in
    let inline =
      String.concat ~sep:""
      @@ List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) inline
    in
    `String inline
  in
  `Object [ ("url", url); ("kind", kind); ("content", inline) ]

let to_json (sidebar : Odoc_document.Sidebar.t) =
  `Array (List.map (Tree.to_json toc_to_json) sidebar)
