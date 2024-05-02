open Odoc_search

let of_entry entry h =
  match Html.url entry with
  | Result.Ok url ->
      let html =
        h
        |> List.map (fun html ->
               Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html)
        |> String.concat ""
      in
      Result.Ok (`Object [ ("url", `String url); ("html", `String html) ])
  | Error _ as e -> e
