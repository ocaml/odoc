let db = Storage_js.load Jv.(to_string @@ get global "sherlodb")

open Brr
open Lwt
open Syntax

let inner_html = El.Prop.jstr (Jstr.v "innerHTML")

let raw_html str =
  let elt = El.div [] in
  El.set_prop inner_html (Jstr.v str) elt ;
  elt

let latest = ref 0
let count = ref 0

let search ~id input _event =
  let query = El.prop El.Prop.value input |> Jstr.to_string in
  let+ pretty_query, results =
    Query.(api ~shards:db { query; packages = []; limit = 50 })
  in
  let results =
    El.[ p [ txt' pretty_query ] ]
    @ List.map
        (fun elt ->
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
                  [ raw_html @@ Format.asprintf "%a" (Tyxml.Html.pp_elt ()) doc
                  ])))
        results
  in
  let results_div =
    Document.find_el_by_id G.document (Jstr.of_string "results") |> Option.get
  in
  if !latest < id
  then begin
    latest := id ;
    El.set_children results_div results
  end

let search input event =
  let id = !count in
  count := id + 1 ;
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () -> search ~id input event)

let main () =
  let search_input =
    Document.find_el_by_id G.document (Jstr.of_string "search") |> Option.get
  in
  Ev.(
    listen input (search search_input)
      (search_input |> El.document |> Document.as_target))

let _ = main ()
