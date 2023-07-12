module Html = Tyxml.Html

let of_entry ({ entry = { id; doc = _; kind = _ }; html } : Entry.with_html) :
    Odoc_html.Json.json =
  let url = Render.url id in

  let html = Html.div ~a:[ Html.a_class [ "search-entry" ] ] html in
  let html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html in
  `Object [ ("url", `String url); ("html", `String html) ]
