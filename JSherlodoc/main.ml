let test = [%blob "odoc_result.db"]
let shards = [ Marshal.from_string test 0 ]

open Brr
open Lwt
open Syntax

let search input _event =
  let query = El.prop El.Prop.value input |> Jstr.to_string in

  let+ pretty_query, results =
    Query.(api ~shards { query; packages = []; limit = 10 })
  in
  let names = List.map (fun r -> r.Db.Elt.name) results in
  let names = String.concat " ; " names in
  let results_div =
    Document.find_el_by_id G.document (Jstr.of_string "results") |> Option.get
  in
  El.set_children results_div El.[ txt' (pretty_query ^ " => " ^ names) ]

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
