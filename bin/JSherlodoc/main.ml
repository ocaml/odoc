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
  | FunctorParameter -> "functor parameter"
  | ModuleSubstitution -> "module subst"
  | ModuleTypeSubstitution -> "module type subst"
  | InstanceVariable -> "instance variable"
  | Val _ -> "val"

let search query =
  let query = query |> Jv.to_jstr |> Jstr.to_string in
  let _pretty_query, results =
    Query.(api ~shards:(Lazy.force db) { query; packages = []; limit = 50 })
  in
  Jv.of_list
    (fun Db.Elt.{ cost = _; name; url; kind; doc; pkg = _ } ->
      let name = Jstr.of_string name in
      let jkind = kind |> string_of_kind |> Jv.of_string in
      let o =
        Jv.(
          obj
            [| "name", of_jstr name
             ; "prefixname", of_string ""
             ; "kind", jkind
             ; "comment", of_string doc.txt
             ; "url", of_string url
            |])
      in
      Db.Elt.(
        match kind with
        | Val { type_; _ } | Constructor { type_; _ } | Field { type_; _ } ->
            Jv.(set o "type" (of_string type_))
        | TypeDecl { type_decl } ->
            (* TODO : remove this hack and switch to real typedecl render *)
            let segments = String.split_on_char '=' type_decl in
            if List.length segments > 1
            then
              let txt =
                segments |> List.tl |> String.concat "=" |> String.trim
              in
              Jv.(set o "type" (of_string txt))
        | _ -> ()) ;
      o)
    results

let main () =
  let module J' = Jstr in
  let o = Jv.callback ~arity:1 search in
  Jv.(set global "odoc_search" o)

let _ = main ()
