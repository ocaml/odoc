
let of_entry { Entry.id; doc = _; kind = _ } h :
    Odoc_html.Json.json =
  let url = Html.url id in
  let html = Tyxml.Html.(div ~a:[ a_class [ "search-entry" ] ] h) in
  let html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html in
  `Object [ ("url", `String url); ("html", `String html) ]
