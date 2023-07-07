module Html = Tyxml.Html

let json_of_namelist li = `Array (List.map (fun str -> `String str) li)

let display_constructor_type args res =
  let open Odoc_model.Lang in
  match args with
  | TypeDecl.Constructor.Tuple args ->
      let type_ =
        match args with
        | _ :: _ :: _ -> TypeExpr.(Arrow (None, Tuple args, res))
        | [ arg ] -> TypeExpr.(Arrow (None, arg, res))
        | _ -> res
      in
      Render.text_of_type type_
  | TypeDecl.Constructor.Record fields ->
      let fields = Render.text_of_record fields in
      let res = Render.text_of_type res in
      fields ^ " -> " ^ res

let typedecl_rhs td =
  let segments = String.split_on_char '=' td.Entry.txt in
  if List.length segments > 1 then
    segments |> List.tl |> String.concat "=" |> String.trim |> ( ^ ) " = "
    |> Option.some
  else None

let constructor_rhs Entry.{ args; res } =
  " : " ^ display_constructor_type args res

let field_rhs Entry.{ mutable_ = _; type_; parent_type = _ } =
  " : " ^ Render.text_of_type type_

let rhs_of_kind extra =
  let open Entry in
  match extra with
  | TypeDecl td -> typedecl_rhs td
  | Constructor cons | ExtensionConstructor cons | Exception cons ->
      Some (constructor_rhs cons)
  | Field field -> Some (field_rhs field)
  | Value { value = _; type_ } -> Some (" : " ^ Render.text_of_type type_)
  | Module | Doc _ | Class_type _ | Method _ | Class _ | TypeExtension _
  | ModuleType ->
      None

let of_strings ~id ~url ~doc ~kind ~rhs : Odoc_html.Json.json =
  let rhs =
    match rhs with None -> [] | Some rhs -> [ ("rhs", `String rhs) ]
  in
  let j_url = `String url in
  let j_id = json_of_namelist id in
  let doc = `String doc in
  let kind = `String kind in
  `Object (rhs @ [ ("id", j_id); ("url", j_url); ("kind", kind); ("doc", doc) ])

let of_entry ({ id; doc; extra; html } : Entry.t) : Odoc_html.Json.json =
  let url = Render.url id in
  let doc = doc |> Render.html_of_doc in
  let kind =
    match extra with
    | TypeDecl _ -> "type"
    | Module -> "module"
    | Value _ -> "value"
    | Exception _ -> "exception"
    | Class_type _ -> "class type"
    | Method _ -> "method"
    | Class _ -> "class"
    | ExtensionConstructor _ -> "extension"
    | ModuleType -> "module type"
    | Doc _ -> "doc"
    | TypeExtension _ -> "type"
    | Constructor _ -> "constructor"
    | Field _ -> "field"
  in
  let kind = Html.div ~a:[ Html.a_class [ "entry-kind" ] ] [ Html.txt kind ] in
  let html =
    Html.div ~a:[ Html.a_class [ "search-entry" ] ] [ kind; html; doc ]
  in
  let html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html in
  `Object [ ("url", `String url); ("html", `String html) ]
