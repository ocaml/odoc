module Storage = Db.Storage
module Succ = Query.Succ
module Sort = Query.Sort

type params =
  { query : string
  ; packages : string list
  ; limit : int
  }

let db_filename = Sys.argv.(1)

let shards =
  let h = Storage.db_open_in db_filename in
  Array.to_list h.Storage.shards

let search query_name query_typ =
  let open Lwt.Syntax in
  let* results_name = Query.find_names ~shards query_name in
  let+ results =
    match query_typ with
    | None -> Lwt.return results_name
    | Some query_typ ->
        let+ results_typ = Query.find_inter ~shards query_typ in
        Succ.inter results_name results_typ
  in
  results

open Lwt.Syntax
module H = Tyxml.Html

let match_packages ~packages { Db.Elt.pkg = package, _version; _ } =
  List.exists (String.equal package) packages

let match_packages ~packages results =
  match packages with
  | [] -> results
  | _ -> Lwt_stream.filter (match_packages ~packages) results

let api params =
  let query_name, query_typ, query_typ_arrow, pretty =
    Query.Parser.of_string params.query
  in
  let* results = search query_name query_typ in
  let results = Succ.to_stream results in
  let results = match_packages ~packages:params.packages results in
  let+ results = Lwt_stream.nget params.limit results in
  let results = Sort.list query_name query_typ_arrow results in
  Ui.render ~pretty results

let api params =
  if String.trim params.query = "" then Lwt.return Ui.explain else api params

open Lwt.Syntax

let get_query params = Option.value ~default:"" (Dream.query params "q")

let get_packages params =
  match Dream.query params "packages" with
  | None -> []
  | Some str -> String.split_on_char ',' str

let get_limit params =
  let default = 100 in
  match Dream.query params "limit" with
  | None -> default
  | Some str -> (
      try max 1 (min default (int_of_string str)) with _ -> default)

let get_params params =
  { query = get_query params
  ; packages = get_packages params
  ; limit = get_limit params
  }

let root fn params =
  let* result = fn params in
  Dream.html result

let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let string_of_tyxml' html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html

let root fn params =
  let params = get_params params in
  try root fn params
  with err ->
    Format.printf "ERROR: %S@." (Printexc.to_string err) ;
    Dream.html (string_of_tyxml @@ Ui.template params.query Ui.explain)

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
  @@ Dream.logger (* @@ cache 3600 *)
  @@ Dream.router
       [ Dream.get "/"
           (root (fun params ->
                let+ result = api params in
                string_of_tyxml @@ Ui.template params.query result))
       ; Dream.get "/api"
           (root (fun params ->
                let+ result = api params in
                string_of_tyxml' result))
       ; Dream.get "/s.css" (Dream.from_filesystem "static" "style.css")
       ; Dream.get "/robots.txt" (Dream.from_filesystem "static" "robots.txt")
       ; Dream.get "/favicon.ico" (Dream.from_filesystem "static" "favicon.ico")
       ]
