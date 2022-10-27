module Storage = Db.Storage
module Succ = Query.Succ
module Sort = Query.Sort

let db_filename = Sys.argv.(1)

let shards =
  let h = Storage.db_open_in db_filename in
  Array.to_list h.Storage.shards

let search (has_typ, query_name, query_typ) =
  let open Lwt.Syntax in
  let* results_name = Query.find_names ~shards query_name in
  let+ results =
    if has_typ
    then
      let+ results_typ = Query.find_inter ~shards query_typ in
      Succ.inter results_name results_typ
    else Lwt.return results_name
  in
  results

open Lwt.Syntax
module H = Tyxml.Html

let api raw_query =
  let has_typ, query_name, query_typ, pretty =
    Query.Parser.of_string raw_query
  in
  let* results = search (has_typ, query_name, query_typ) in
  let+ results = Succ.to_list results in
  let results = Sort.by_name query_name results in
  Ui.render ~pretty results

let api query =
  if String.trim query = "" then Lwt.return Ui.explain else api query

open Lwt.Syntax

let get_query params = Option.value ~default:"" (Dream.query params "q")

let root ~query fn _params =
  let* result = fn query in
  Dream.html result

let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let string_of_tyxml' html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html

let root fn params =
  let query = get_query params in
  try root ~query fn params
  with err ->
    Format.printf "ERROR: %S@." (Printexc.to_string err) ;
    Dream.html (string_of_tyxml @@ Ui.template query Ui.explain)

let root fn params =
  try root fn params
  with _ -> Dream.html (string_of_tyxml @@ Ui.template "" Ui.explain)

let cache : int -> Dream.middleware =
 fun max_age f req ->
  let+ response = f req in
  Dream.add_header response "Cache-Control"
    ("public, max-age=" ^ string_of_int max_age) ;
  response

let () =
  Dream.run ~interface:"127.0.0.1" ~port:1234
  @@ Dream.logger @@ cache 3600
  @@ Dream.router
       [ Dream.get "/"
           (root (fun q ->
                let+ result = api q in
                string_of_tyxml @@ Ui.template q result))
       ; Dream.get "/api"
           (root (fun q ->
                let+ result = api q in
                string_of_tyxml' result))
       ; Dream.get "/s.css" (Dream.from_filesystem "static" "style.css")
       ; Dream.get "/robots.txt" (Dream.from_filesystem "static" "robots.txt")
       ; Dream.get "/favicon.ico" (Dream.from_filesystem "static" "favicon.ico")
       ]
