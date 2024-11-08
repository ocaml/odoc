open Odoc_search

let of_entry entry h =
  let url = Html.url entry in
  let html =
    h
    |> List.map (fun html -> Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html)
    |> String.concat ""
  in
  `Object [ ("url", `String url); ("html", `String html) ]
