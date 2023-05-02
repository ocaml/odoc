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
  | Val { str_type; _ } ->
      [ txt' "val "; em [ txt' elt.Db.Elt.name ]; txt' " : "; txt' str_type ]
  | Doc -> [ txt' "Doc "; em [ txt' elt.Db.Elt.name ] ]
  | TypeDecl { html = type_decl } ->
      [ txt' "type "; em [ txt' elt.Db.Elt.name ]; txt' " : "; txt' type_decl ]
  | Module -> [ txt' "Module "; em [ txt' elt.Db.Elt.name ] ]
  | Exception -> [ txt' "Exception "; em [ txt' elt.Db.Elt.name ] ]
  | Class_type -> [ txt' "Class_type "; em [ txt' elt.Db.Elt.name ] ]
  | Method -> [ txt' "Method "; em [ txt' elt.Db.Elt.name ] ]
  | Class -> [ txt' "Class "; em [ txt' elt.Db.Elt.name ] ]
  | TypeExtension -> [ txt' "TypeExtension "; em [ txt' elt.Db.Elt.name ] ]
  | ExtensionConstructor ->
      [ txt' "ExtensionConstructor "; em [ txt' elt.Db.Elt.name ] ]
  | ModuleType -> [ txt' "ModuleType "; em [ txt' elt.Db.Elt.name ] ]
  | Constructor -> [ txt' "Constructor "; em [ txt' elt.Db.Elt.name ] ]
  | Field -> [ txt' "Field "; em [ txt' elt.Db.Elt.name ] ]
  | FunctorParameter ->
      [ txt' "FunctorParameter "; em [ txt' elt.Db.Elt.name ] ]
  | ModuleSubstitution ->
      [ txt' "ModuleSubstitution "; em [ txt' elt.Db.Elt.name ] ]
  | ModuleTypeSubstitution ->
      [ txt' "ModuleTypeSubstitution "; em [ txt' elt.Db.Elt.name ] ]
  | InstanceVariable ->
      [ txt' "InstanceVariable "; em [ txt' elt.Db.Elt.name ] ]

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
