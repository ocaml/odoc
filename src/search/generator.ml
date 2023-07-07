module Html = Tyxml.Html

open Odoc_model

let constructor id _args _res =
  let name = Paths.Identifier.name id in
  Format.sprintf "constructor %s : " name

let type_from_path : Paths.Path.Type.t -> string =
 fun path ->
  match path with
  | `Identifier (id, _) -> Paths.Identifier.name id
  | `Dot (_prefix, suffix) -> Format.sprintf "%s" suffix
  | `Resolved _ when Paths.Path.is_hidden (path :> Paths.Path.t) -> "TODO"
  | `Resolved rp ->
      let id = Paths.Path.Resolved.identifier (rp :> Paths.Path.Resolved.t) in
      let name = Paths.Identifier.name id in
      Format.sprintf "%s" name

let rec format_type_path ~delim (params : Odoc_model.Lang.TypeExpr.t list) path
    =
  let enclose =
    match delim with
    | `brackets -> Format.sprintf "(%s)"
    | _ -> Format.sprintf "[%s]"
  in
  match params with
  | [] -> path
  | [ param ] ->
      let args = type_expr ~needs_parentheses:true param in
      Format.sprintf "%s %s" args path
  | params ->
      let params = List.map type_expr params in
      let args = Format.sprintf "(%s)" (String.concat ", " params) in
      enclose @@ Format.sprintf "%s %s" args path

and type_expr ?(needs_parentheses = false) (t : Odoc_model.Lang.TypeExpr.t) =
  match t with
  | Var s -> Format.sprintf "'%s" s
  | Any -> "_"
  | Alias (te, alias) ->
      Printf.sprintf "%s as %s" (type_expr ~needs_parentheses:true te) alias
  | Arrow (None, src, dst) ->
      Printf.sprintf "%s -> %s"
        (type_expr ~needs_parentheses:true src)
        (type_expr dst)
  | Arrow (Some (Label lbl), src, dst) ->
      let res =
        Printf.sprintf "%s:%s -> %s" lbl
          (type_expr ~needs_parentheses:true src)
          (type_expr dst)
      in
      if not needs_parentheses then res else "(" ^ res ^ ")"
  | Arrow (Some (Optional lbl), src, dst) ->
      let res =
        Printf.sprintf "?%s:%s -> %s" lbl
          (type_expr ~needs_parentheses:true src)
          (type_expr dst)
      in
      if not needs_parentheses then res else "(" ^ res ^ ")"
  | Tuple lst ->
      let ts = List.map (type_expr ~needs_parentheses:true) lst in
      let res = String.concat " * " ts in
      if not needs_parentheses then res else "(" ^ res ^ ")"
  | Constr (args, link) ->
      format_type_path ~delim:`parens link (type_from_path args)
  | Polymorphic_variant _ -> "TODO"
  | Object _ -> "TODO"
  | Class (_, _) -> "TODO"
  | Poly (_, _) -> "TODO"
  | Package _ -> "TODO"

let format_params :
    ?delim:[ `parens | `brackets ] ->
    Odoc_model.Lang.TypeDecl.param list ->
    string =
 fun ?(delim = `parens) params ->
  let format_param { Odoc_model.Lang.TypeDecl.desc; variance; injectivity } =
    let desc =
      match desc with
      | Odoc_model.Lang.TypeDecl.Any -> [ "_" ]
      | Var s -> [ "'"; s ]
    in
    let var_desc =
      match variance with
      | None -> desc
      | Some Odoc_model.Lang.TypeDecl.Pos -> "+" :: desc
      | Some Odoc_model.Lang.TypeDecl.Neg -> "-" :: desc
    in
    let final = if injectivity then "!" :: var_desc else var_desc in
    String.concat "" final
  in
  match params with
  | [] -> ""
  | [ x ] -> format_param x
  | lst -> (
      let params = String.concat ", " (List.map format_param lst) in
      (match delim with `parens -> "(" | `brackets -> "[")
      ^ params
      ^ match delim with `parens -> ")" | `brackets -> "]")

let html_of_entry (entry : Odoc_model.Fold.item) =
  let content =
    match entry with
    | CompilationUnit { content = Module _; id; _ } ->
        let name = Paths.Identifier.name id in
        Printf.sprintf "module %s = struct ... end" name
    | CompilationUnit { content = Pack _; _ } -> ""
    | TypeDecl t ->
        let tyname = Paths.Identifier.name t.id in
        let repr =
          match t.representation with
          | None -> ""
          | Some repr -> (
              match repr with
              | Extensible -> " = .."
              | Variant _ -> " = <variant>"
              | Record _ -> " = <record>")
        in
        Format.sprintf "type %s%s" tyname repr
    | Module t ->
        let modname = Paths.Identifier.name t.id in
        Format.sprintf "module %s : sig ... end" modname
    | Value t ->
        let external_ =
          match t.value with Abstract -> "" | External _ -> "external "
        in
        let name = Paths.Identifier.name t.id in
        Format.sprintf "%sval %s : %s" external_ name (type_expr t.type_)
    | Exception t ->
        let cstr = constructor (t.id :> Paths.Identifier.t) t.args t.res in
        Format.sprintf "exception %s" cstr
    | ClassType t ->
        let name = Paths.Identifier.name t.id in
        let params = format_params ~delim:`brackets t.params in
        let virtual_ = if t.virtual_ then "virtual " else "" in
        Format.sprintf "class type %s%s%s = object ... end" virtual_ params name
    | Method t ->
        let name = Paths.Identifier.name t.id in
        let virtual_ = if t.virtual_ then "virtual " else "" in
        let private_ = if t.private_ then "private " else "" in
        Format.sprintf "val %s%s%s : %s" private_ virtual_ name
          (type_expr t.type_)
    | Class t ->
        let name = Paths.Identifier.name t.id in
        let params =
          match t.params with
          | [] -> ""
          | _ :: _ as params -> format_params ~delim:`brackets params ^ " "
        in
        let virtual_ = if t.virtual_ then "virtual" else "" in
        Format.sprintf "class %s%s%s = object ... end" virtual_ params name
    | Extension t ->
        let name = type_from_path t.type_path in
        Format.sprintf "type %s += ..." name
    | ModuleType t ->
        let modname = Paths.Identifier.name t.id in
        Format.sprintf "module type %s = sig ... end" modname
    | Doc _ -> ""
  in
  Html.div ~a:[] [ Html.txt content ]

let html_of_doc doc =
  let config =
    Odoc_html.Config.v ~search_result:true ~semantic_uris:false ~indent:false
      ~flat:false ~open_details:false ~as_json:false ~search_files:[] ()
  in
  Tyxml.Html.div ~a:[]
  @@ Odoc_html.Generator.doc ~config ~xref_base_uri:""
  @@ Odoc_document.Comment.to_ir doc
