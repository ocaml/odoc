type html = Html_types.div_content Tyxml.Html.elt

open Odoc_model
open Lang
open Odoc_index

let url { Entry.id; kind; doc = _ } =
  let open Entry in
  let stop_before =
    (* Some module/module types/... might not have an expansion, so we need to
       be careful and set [stop_before] to [true] for those kind of search
       entries, to avoid linking to an inexistant page.
    *)
    match kind with
    | Module { has_expansion = false; _ }
    | ModuleType { has_expansion = false; _ } ->
        true
    | _ -> false
  in
  match Odoc_document.Url.from_identifier ~stop_before id with
  | Ok url ->
      let config =
        Odoc_html.Config.v ~search_result:true ~semantic_uris:false
          ~indent:false ~flat:false ~open_details:false ~as_json:false ~remap:[]
          ()
      in
      let url = Odoc_html.Link.href ~config ~resolve:(Base "") url in
      Result.Ok url
  | Error _ as e -> e

let map_option f = function Some x -> Some (f x) | None -> None

let display_constructor_args args =
  let open Odoc_model.Lang in
  match args with
  | TypeDecl.Constructor.Tuple args ->
      (match args with
      | _ :: _ :: _ -> Some TypeExpr.(Tuple args)
      | [ arg ] -> Some arg
      | _ -> None)
      |> map_option Text.of_type
  | TypeDecl.Constructor.Record fields -> Some (Text.of_record fields)

let constructor_rhs ~args ~res =
  let args = display_constructor_args args in
  let res = map_option Text.of_type res in
  match (args, res) with
  | None, None -> ""
  | None, Some res -> " : " ^ res
  | Some args, None -> " of " ^ args
  | Some args, Some res -> " : " ^ args ^ " -> " ^ res

let field_rhs ({ mutable_ = _; type_; parent_type = _ } : Entry.field_entry) =
  " : " ^ Text.of_type type_

let typedecl_params ?(delim = `parens) params =
  let format_param { TypeDecl.desc; variance; injectivity } =
    let desc =
      match desc with TypeDecl.Any -> [ "_" ] | Var s -> [ "'"; s ]
    in
    let var_desc =
      match variance with
      | None -> desc
      | Some TypeDecl.Pos -> "+" :: desc
      | Some TypeDecl.Neg -> "-" :: desc
    in
    let final = if injectivity then "!" :: var_desc else var_desc in
    String.concat "" final
  in
  match params with
  | [] -> None
  | [ x ] -> Some (format_param x)
  | lst ->
      let params = String.concat ", " (List.map format_param lst) in
      Some
        ((match delim with `parens -> "(" | `brackets -> "[")
        ^ params
        ^ match delim with `parens -> ")" | `brackets -> "]")

let type_decl_constraint (typ, typ') =
  "constraint" ^ " " ^ Text.of_type typ ^ " = " ^ Text.of_type typ'

let typedecl_params_of_entry ({ kind; _ } : Entry.t) =
  match kind with
  | Entry.TypeDecl { canonical = _; equation; representation = _ } ->
      typedecl_params equation.params
  | _ -> None

let typedecl_repr ~private_ (repr : TypeDecl.Representation.t) =
  let constructor ~id ~args ~res =
    let name = Comment.Identifier.name id in
    name ^ constructor_rhs ~args ~res
  in
  let private_ = if private_ then "private " else "" in
  " = " ^ private_
  ^
  match repr with
  | Extensible -> ".."
  | Variant constructors ->
      constructors
      |> List.map (fun ({ id; args; res; _ } : TypeDecl.Constructor.t) ->
             constructor ~id ~args ~res)
      |> String.concat " | "
  | Record record -> Text.of_record record

let typedecl_rhs ({ equation; representation; _ } : Entry.type_decl_entry) =
  let ({ private_; manifest; constraints; _ } : TypeDecl.Equation.t) =
    equation
  in
  let repr =
    match representation with Some r -> typedecl_repr ~private_ r | None -> ""
  in
  let manifest =
    match manifest with None -> "" | Some typ -> " = " ^ Text.of_type typ
  in
  let constraints =
    match constraints with
    | [] -> ""
    | _ :: _ ->
        " " ^ (constraints |> List.map type_decl_constraint |> String.concat " ")
  in
  match repr ^ manifest ^ constraints with "" -> None | r -> Some r

let constructor_rhs ({ args; res } : Entry.constructor_entry) =
  constructor_rhs ~args ~res:(Some res)

(** Kinds *)

let kind_doc = "doc"

let kind_typedecl = "type"

let kind_module = "mod"

let kind_exception = "exn"

let kind_class_type = "class"
let kind_class = "class"

let kind_method = "meth"

let kind_extension_constructor = "cons"

let kind_module_type = "sig"

let kind_constructor = "cons"

let kind_field = "field"

let kind_value = "val"

let kind_extension = "ext"

let string_of_kind =
  let open Entry in
  function
  | Constructor _ -> kind_constructor
  | Field _ -> kind_field
  | ExtensionConstructor _ -> kind_extension_constructor
  | TypeDecl _ -> kind_typedecl
  | Module _ -> kind_module
  | Value _ -> kind_value
  | Exception _ -> kind_exception
  | Class_type _ -> kind_class_type
  | Method _ -> kind_method
  | Class _ -> kind_class
  | TypeExtension _ -> kind_extension
  | ModuleType _ -> kind_module_type
  | Doc -> kind_doc

let value_rhs (t : Entry.value_entry) = " : " ^ Text.of_type t.type_

let of_strings ~kind ~prefix_name ~name ~rhs ~typedecl_params ~doc =
  [
    Tyxml.Html.Unsafe.data
      (Odoc_html_frontend.of_strings ~kind ~prefix_name ~name ~rhs
         ~typedecl_params ~doc);
  ]
let rhs_of_kind (entry : Entry.kind) =
  match entry with
  | TypeDecl td -> typedecl_rhs td
  | Value t -> Some (value_rhs t)
  | Constructor t | ExtensionConstructor t | Exception t ->
      Some (constructor_rhs t)
  | Field f -> Some (field_rhs f)
  | Module _ | Class_type _ | Method _ | Class _ | TypeExtension _
  | ModuleType _ | Doc ->
      None

let names_of_id id =
  let fullname = Paths.Identifier.fullname id in
  let prefix_name, name =
    let rev_fullname = List.rev fullname in
    ( rev_fullname |> List.tl |> List.rev
      |> List.map (fun str -> str ^ ".")
      |> String.concat "",
      List.hd rev_fullname )
  in
  (prefix_name, name)

let of_doc doc =
  let config =
    Odoc_html.Config.v ~search_result:true ~semantic_uris:false ~indent:false
      ~flat:false ~open_details:false ~as_json:false ~remap:[] ()
  in
  Tyxml.Html.div ~a:[]
  @@ Odoc_html.Generator.doc ~config ~xref_base_uri:""
  @@ Odoc_document.Comment.to_ir doc

let html_string_of_doc doc =
  doc |> of_doc |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let of_entry (entry : Entry.t) =
  let ({ id; doc; kind } : Entry.t) = entry in
  let rhs = rhs_of_kind kind in
  let prefix_name, name = names_of_id id in
  let prefix_name = Some prefix_name and name = Some name in
  let doc = html_string_of_doc doc in
  let kind = string_of_kind kind in
  let typedecl_params = typedecl_params_of_entry entry in
  of_strings ~kind ~prefix_name ~name ~rhs ~doc ~typedecl_params
