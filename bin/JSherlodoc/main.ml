let db =
  lazy (Storage_js.load Jv.(to_string @@ call global "sherlodoc_db" [||]))

let string_of_kind (kind : Db.Elt.kind) =
  let open Db.Elt in
  match kind with
  | Doc -> "doc"
  | TypeDecl _ -> "type"
  | Module -> "module"
  | Exception -> "exception"
  | Class_type -> "class type"
  | Method -> "method"
  | Class -> "class"
  | TypeExtension -> "type ext"
  | ExtensionConstructor -> "extension constructor"
  | ModuleType -> "module type"
  | Constructor _ -> "constructor"
  | Field _ -> "field"
  | Val _ -> "val"

let search message =
  let query = Jv.get message "data" in
  let query = query |> Jv.to_jstr |> Jstr.to_string in
  let _pretty_query, results =
    Query.(api ~shards:(Lazy.force db) { query; packages = []; limit = 50 })
  in
  Jv.(apply (get global "postMessage"))
    [| Jv.of_list
         (fun Db.Elt.{ json_display; _ } ->
           json_display |> Jstr.of_string |> Brr.Json.decode |> Result.get_ok)
         results
    |]

let main () =
  let module J' = Jstr in
  let o = Jv.callback ~arity:1 search in
  Jv.(set global "onmessage" o)

let _ = main ()
