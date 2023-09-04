let of_entry { Entry.id; doc = _; kind = _ } h : Odoc_html.Json.json =
  let url = Html.url id in
  let html =
    h
    |> List.map (fun html -> Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html)
    |> String.concat "\n"
  in
  `Object [ ("url", `String url); ("html", `String html) ]
