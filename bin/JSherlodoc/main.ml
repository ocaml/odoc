let db = Storage_js.load Jv.(to_string @@ get global "sherlodb")

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
  | Constructor -> "constructor"
  | Field -> "field"
  | FunctorParameter -> "functor parameter"
  | ModuleSubstitution -> "module subst"
  | ModuleTypeSubstitution -> "module type subst"
  | InstanceVariable -> "instance variable"
  | Val _ -> "val"

let search query =
  let query = query |> Jv.to_jstr |> Jstr.to_string in
  let _pretty_query, results =
    Query.(api ~shards:db { query; packages = []; limit = 50 })
  in

  Jv.of_list
    (fun Db.Elt.{ cost = _; name; url; kind; doc; pkg = _ } ->
      let name = Jstr.of_string name in
      let kind = kind |> string_of_kind |> Jv.of_string in
      Jv.(
        obj
          [| "name", of_jstr name
           ; "prefixname", of_string ""
           ; "kind", kind
           ; "comment", of_string doc.txt
           ; "url", of_string url
          |]))
    results

let main () =
  let module J' = Jstr in
  let o = Jv.callback ~arity:1 search in
  Jv.(set global "odoc_search" o)

let _ = main ()
