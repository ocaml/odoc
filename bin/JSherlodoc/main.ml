let db = Storage_js.load Jv.(to_string @@ get global "sherlodb")

open Brr

let inner_html = El.Prop.jstr (Jstr.v "innerHTML")

let raw_html str =
  let elt = El.div ~at:At.[ class' (Jstr.of_string "docstring") ] [] in
  El.set_prop inner_html (Jstr.v str) elt ;
  elt

let latest = ref 0
let count = ref 1

let render_elt elt =
  let open Db.Elt in
  let open El in
  match elt.kind with
  | Db.Elt.Val { str_type; _ } ->
      [ txt' "val "; em [ txt' elt.Db.Elt.name ]; txt' " : "; txt' str_type ]
  | Db.Elt.Type ->
      [ txt' "type "; em [ txt' elt.Db.Elt.name ]; txt' " : "; txt' "WIP" ]
  | Db.Elt.Module -> [ txt' "module "; em [ txt' elt.Db.Elt.name ] ]

let search ~id input =
  let query = El.prop El.Prop.value input |> Jstr.to_string in
  let _pretty_query, results =
    Query.(api ~shards:db { query; packages = []; limit = 50 })
  in
  let results = List.of_seq @@ Seq.take 10 @@ List.to_seq results in
  let results =
    List.map
      (fun elt ->
        El.(
          div
            ~at:At.[ class' (Jstr.of_string "result") ]
            ([ code (render_elt elt) ]
            @
            match elt.Db.Elt.doc with
            | None -> []
            | Some doc -> [ raw_html doc ])))
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

let search input =
  let id = !count in
  count := id + 1 ;
  search ~id input

let main () =
  let search_input =
    Document.find_el_by_id G.document (Jstr.of_string "search") |> Option.get
  in
  let _ =
    Ev.(
      listen input
        (fun _ -> search search_input)
        (search_input |> El.document |> Document.as_target))
  in
  search search_input

let _ = main ()
