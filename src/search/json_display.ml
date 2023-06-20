module Html = Tyxml.Html

let display_expression_rhs args res =
  let open Odoc_model.Lang in
  match res with
  | Some res -> (
      " : "
      ^
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
          fields ^ " -> " ^ res)
  | None -> (
      match args with
      | TypeDecl.Constructor.Tuple args -> (
          match args with
          | _ :: _ :: _ -> " of " ^ Render.text_of_type (TypeExpr.Tuple args)
          | [ arg ] -> " of " ^ Render.text_of_type arg
          | _ -> "")
      | TypeDecl.Constructor.Record fields ->
          let fields = Render.text_of_record fields in
          " of " ^ fields)
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

let of_entry ({ id; doc; extra } : Entry.t) : Odoc_html.Json.json =
  let j_url = `String (Render.url id) in
  let kind_s =
    match extra with
    | TypeDecl _ -> "type"
    | Module -> "module"
    | Value _ -> "val"
    | Doc _ -> "doc"
    | Exception _ -> "exn"
    | Class_type _ -> "class type"
    | Method _ -> "method"
    | Class _ -> "class"
    | TypeExtension _ -> "type ext"
    | ExtensionConstructor _ -> "extension constructor"
    | ModuleType -> "module type"
    | Constructor _ -> "constructor"
    | Field _ -> "field"
  in
  let kind_html =
    Html.span ~a:[ Html.a_class [ "entry-kind" ] ] [ Html.txt kind_s ]
  in
  let rhs =
    let s =
      match extra with
      | TypeDecl { canonical = _; equation = _; representation = _; txt } ->
          let segments = String.split_on_char '=' txt in
          if List.length segments > 1 then
            segments |> List.tl |> String.concat "=" |> String.trim
            |> ( ^ ) " = " |> Option.some
          else None
      | Constructor { args; res } ->
          Some (" : " ^ display_constructor_type args res)
      | Field { mutable_ = _; type_; parent_type = _ } ->
          Some (" : " ^ Render.text_of_type type_)
      | Exception { args; res } -> Some (display_expression_rhs args res)
      | Value { value = _; type_ } -> Some (" : " ^ Render.text_of_type type_)
      | Module | Doc _ | Class_type _ | Method _ | Class _ | TypeExtension _
      | ExtensionConstructor _ | ModuleType ->
          None
    in
    match s with
    | None -> []
    | Some s -> [ Html.code ~a:[ Html.a_class [ "entry-rhs" ] ] [ Html.txt s ] ]
  in
  let title =
    let prefixname, name =
      let rec loop acc id =
        match id with
        | [] -> ("", "")
        | [ name ] -> (acc, name)
        | hd :: tl -> loop (acc ^ hd ^ ".") tl
      in
      let prefixname, name =
        loop "" (Odoc_model.Paths.Identifier.fullname id)
      in
      ( Html.span ~a:[ Html.a_class [ "prefix-name" ] ] [ Html.txt prefixname ],
        Html.span ~a:[ Html.a_class [ "entry-name" ] ] [ Html.txt name ] )
    in
    Html.code
      ~a:[ Html.a_class [ "entry-title" ] ]
      ([ kind_html; prefixname; name ] @ rhs)
  in
  let comment =
    let doc = Render.html_of_doc doc in
    Html.div ~a:[ Html.a_class [ "entry-comment" ] ] [ doc ]
  in
  let container =
    let class_ = String.map (function ' ' -> '-' | c -> c) kind_s in
    Html.div
      ~a:[ Html.a_class [ "search-entry"; class_ ] ]
      (title :: [ comment ])
  in
  `Object
    [
      ("html", `String (Format.asprintf "%a" (Tyxml.Html.pp_elt ()) container));
      ("url", j_url);
    ]
