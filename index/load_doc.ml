module Entry = Db.Entry
module Db_common = Db
module ModuleName = Odoc_model.Names.ModuleName

let generic_cost ~ignore_no_doc name has_doc =
  (* name length is important not because short identifier are better in the
     abstract, but because the shortest result will be close to the query, as
     the suffix tree does not return results shorter than the query*)
  (String.length name * 6)
  (* + (5 * List.length path) TODO : restore depth based ordering *)
  + (if ignore_no_doc || has_doc then 0 else 400)
  + if String.starts_with ~prefix:"Stdlib." name then 0 else 100

let kind_cost = function
  | Entry.Kind.Doc -> 400
  | _ -> 0

let cost ~name ~kind ~doc_html ~rhs =
  let ignore_no_doc =
    match kind with
    | Entry.Kind.Module | Module_type -> true
    | _ -> false
  in
  let has_doc = doc_html <> "" in
  generic_cost ~ignore_no_doc name has_doc
  + kind_cost kind
  + String.length (Option.value ~default:"" rhs)

let string_of_html = Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let rec typ_of_odoc_typ otyp =
  let open Db.Typexpr in
  match otyp with
  | Odoc_model.Lang.TypeExpr.Var _str -> any
  | Any -> any
  | Arrow (_lbl, left, right) -> arrow (typ_of_odoc_typ left) (typ_of_odoc_typ right)
  | Constr (name, args) ->
    constr (Typename.to_string name) (List.map typ_of_odoc_typ args)
  | Tuple li -> tuple (List.map typ_of_odoc_typ li)
  | _ -> unhandled

let with_tokenizer str fn =
  let str = String.lowercase_ascii str in
  let buf = Buffer.create 16 in
  let flush () =
    let word = Buffer.contents buf in
    if word <> "" then fn word ;
    Buffer.clear buf
  in
  let rec go i =
    if i >= String.length str
    then flush ()
    else (
      let chr = str.[i] in
      if (chr >= 'a' && chr <= 'z')
         || (chr >= '0' && chr <= '9')
         || chr = '_'
         || chr = '@'
      then Buffer.add_char buf chr
      else flush () ;
      go (i + 1))
  in
  go 0

let register_doc ~db elt doc_txt =
  with_tokenizer doc_txt @@ fun word -> Db.store_word db word elt

let register_full_name ~db (elt : Db.Entry.t) =
  let name = String.lowercase_ascii elt.name in
  Db.store_word db name elt

let searchable_type_of_constructor args res =
  let open Odoc_model.Lang in
  match args with
  | TypeDecl.Constructor.Tuple args ->
    (match args with
     | _ :: _ :: _ -> TypeExpr.(Arrow (None, Tuple args, res))
     | [ arg ] -> TypeExpr.(Arrow (None, arg, res))
     | _ -> res)
  | TypeDecl.Constructor.Record fields ->
    List.fold_left
      (fun res field ->
        let open TypeDecl.Field in
        let field_name = Odoc_model.Paths.Identifier.name field.id in
        TypeExpr.Arrow (Some (Label field_name), field.type_, res))
      res
      fields

let searchable_type_of_record parent_type type_ =
  let open Odoc_model.Lang in
  TypeExpr.Arrow (None, parent_type, type_)

let convert_kind (Odoc_search.Entry.{ kind; _ } as entry) =
  let open Odoc_search.Entry in
  match kind with
  | TypeDecl _ -> Entry.Kind.Type_decl (Odoc_search.Html.typedecl_params_of_entry entry)
  | Module -> Entry.Kind.Module
  | Value { value = _; type_ } ->
    let typ = typ_of_odoc_typ type_ in
    Entry.Kind.Val typ
  | Constructor { args; res } ->
    let searchable_type = searchable_type_of_constructor args res in
    let typ = typ_of_odoc_typ searchable_type in
    Entry.Kind.Constructor typ
  | Field { mutable_ = _; parent_type; type_ } ->
    let typ = type_ |> searchable_type_of_record parent_type |> typ_of_odoc_typ in
    Entry.Kind.Field typ
  | Doc _ -> Doc
  | Exception { args; res } ->
    let searchable_type = searchable_type_of_constructor args res in
    let typ = typ_of_odoc_typ searchable_type in
    Entry.Kind.Exception typ
  | Class_type _ -> Class_type
  | Method _ -> Method
  | Class _ -> Class
  | TypeExtension _ -> Type_extension
  | ExtensionConstructor { args; res } ->
    let searchable_type = searchable_type_of_constructor args res in
    let typ = typ_of_odoc_typ searchable_type in
    Entry.Kind.Extension_constructor typ
  | ModuleType -> Module_type

let register_type_expr ~db elt typ =
  let type_polarities = Db.Type_polarity.of_typ ~any_is_poly:true ~all_names:true typ in
  Db.store_type_polarities db elt type_polarities

let register_kind ~db elt =
  let open Db.Entry in
  match Kind.get_type elt.kind with
  | None -> ()
  | Some typ -> register_type_expr ~db elt typ

let rec is_from_module_type (id : Odoc_model.Paths.Identifier.Any.t) =
  let open Odoc_model.Paths in
  match id.iv with
  | `CoreType _ | `CoreException _ | `Root _ | `Page _ | `LeafPage _ -> false
  | `ModuleType _ -> true
  | #Identifier.NonSrc.t_pv as x ->
    let parent = Identifier.label_parent { id with iv = x } in
    is_from_module_type (parent :> Identifier.Any.t)
  | _ -> false

let is_from_module_type Odoc_search.Entry.{ id; _ } =
  match id.iv with
  | `ModuleType (parent, _) ->
    (* A module type itself is not *from* a module type, but it might be if one
       of its parents is a module type. *)
    is_from_module_type (parent :> Odoc_model.Paths.Identifier.Any.t)
  | _ -> is_from_module_type id

let prefixname id =
  let parts = Odoc_model.Paths.Identifier.fullname id in
  match List.rev parts with
  | [] -> ""
  | _ :: prefix -> String.concat "." (List.rev prefix)

let register_entry
  ~db
  ~index_name
  ~type_search
  ~index_docstring
  ~pkg
  (Odoc_search.Entry.{ id; doc; kind } as entry)
  =
  let module Sherlodoc_entry = Entry in
  let open Odoc_search in
  let open Odoc_search.Entry in
  let is_type_extension =
    match kind with
    | TypeExtension _ -> true
    | _ -> false
  in
  if Odoc_model.Paths.Identifier.is_internal id || is_type_extension
  then ()
  else begin
    let full_name = id |> Odoc_model.Paths.Identifier.fullname |> String.concat "." in
    let doc_txt = Text.of_doc doc in
    let doc_html = doc |> Html.of_doc |> string_of_html in
    let doc_html =
      match doc_txt with
      | "" -> ""
      | _ -> doc_html
    in
    let rhs = Html.rhs_of_kind kind in
    let kind = convert_kind entry in
    let name =
      match kind with
      | Doc -> prefixname id
      | _ -> full_name
    in
    let cost = cost ~name ~kind ~doc_html ~rhs in
    let url = Html.url id in
    let url = Result.get_ok url in
    let is_from_module_type = is_from_module_type entry in
    let elt =
      Sherlodoc_entry.v ~name ~kind ~rhs ~doc_html ~cost ~url ~is_from_module_type ~pkg ()
    in
    if index_docstring then register_doc ~db elt doc_txt ;
    if index_name && kind <> Doc then register_full_name ~db elt ;
    if type_search then register_kind ~db elt
  end
