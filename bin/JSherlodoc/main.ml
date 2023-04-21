let db = Storage_js.load Jv.(to_string @@ get global "sherlodb")

open Brr
open Lwt
open Syntax

let search input _event =
  let query = El.prop El.Prop.value input |> Jstr.to_string in
  let+ _pretty_query, results =
    Query.(api ~shards:db { query; packages = []; limit = 50 })
  in
  let results =
    results
    |> List.map (fun elt ->
           El.(
             div
               ([ p
                    ~at:At.[ style (Jstr.of_string "color:red") ]
                    [ txt' elt.Db.Elt.name ]
                ]
               @
               match elt.Db.Elt.doc with
               | None -> []
               | Some doc ->
                   [ p
                       [ txt' @@ Format.asprintf "%a" (Tyxml.Html.pp_elt ()) doc
                       ]
                   ])))
  in

  let results_div =
    Document.find_el_by_id G.document (Jstr.of_string "results") |> Option.get
  in
  El.set_children results_div results

let search input event =
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () -> search input event)

let main () =
  let search_input =
    Document.find_el_by_id G.document (Jstr.of_string "search") |> Option.get
  in
  Ev.(
    listen input (search search_input)
      (search_input |> El.document |> Document.as_target))

let _ = main ()
