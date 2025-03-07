open Odoc_utils

let toc_to_json
    ({ url; valid_link; content = _; _ } : Odoc_document.Sidebar.entry) :
    Json.json =
  (* let config =
    Config.v ~semantic_uris:true ~indent:true ~flat:false ~open_details:false
      ~as_json:true ~remap:[] ()
  in *)
  let url, kind =
    match valid_link with
    | false -> (`Null, `Null)
    | true ->
        let _href = Link.href ~resolve:(Link.Base "") url in
        let kind =
          Format.asprintf "%a" Odoc_document.Url.Anchor.pp_kind url.kind
        in

        (`String "TODO", `String kind)
  in
  let inline = `String "TODO" in
  `Object [ ("url", url); ("kind", kind); ("content", inline) ]

let to_json (sidebar : Odoc_document.Sidebar.t) =
  `Array (List.map (Tree.to_json toc_to_json) sidebar)
