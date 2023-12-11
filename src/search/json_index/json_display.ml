open Odoc_search

let of_entry { Entry.id; doc = _; kind = _ } h =
  match Html.url id with
  | Result.Ok url ->
      let html =
        h
        |> List.map (fun html ->
               Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html)
        |> String.concat ""
      in
      Result.Ok (`Object [ ("url", `String url); ("html", `String html) ])
  | Error _ as e -> e
