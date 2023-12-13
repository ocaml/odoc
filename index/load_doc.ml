module Elt = Db.Elt
module Db_common = Db
module Types = Db.Types
module ModuleName = Odoc_model.Names.ModuleName

let generic_cost ~ignore_no_doc name has_doc =
  (* name length is important not because short identifier are better in the
     abstract, but because the shortest result will be close to the query, as
     the suffix tree does not return results shorter than the query*)
  (String.length name * 6)
  (* + (5 * List.length path) TODO : restore depth based ordering *)
  + (if ignore_no_doc || has_doc then 0 else 30)
  + if String.starts_with ~prefix:"Stdlib." name then -100 else 0

let type_cost paths =
  paths |> List.concat |> List.map String.length |> List.fold_left ( + ) 0

let kind_cost (kind : Elt.Kind.t) =
  match kind with
  | Constructor type_path | Field type_path | Val type_path ->
      type_cost type_path
  | Doc -> 400
  | TypeDecl _ | Module -> 0
  | Exception _ | Class_type | Method | Class -> 10
  | TypeExtension -> 1000
  | ExtensionConstructor _ | ModuleType -> 10

let cost ~name ~kind ~doc_html =
  let ignore_no_doc =
    match kind with
    | Elt.Kind.Module | ModuleType -> true
    | _ -> false
  in
  let has_doc = doc_html <> "" in
  generic_cost ~ignore_no_doc name has_doc + kind_cost kind

(*

  todo : check usefulness
  let rec type_size = function
    | Odoc_model.Lang.TypeExpr.Var _ -> 1
    | Any -> 1
    | Arrow (lbl, a, b) ->
        (match lbl with
        | None -> 0
        | Some _ -> 1)
        + type_size a + type_size b
    | Constr (_, args) -> List.fold_left (fun acc t -> acc + type_size t) 1 args
    | Tuple args -> List.fold_left (fun acc t -> acc + type_size t) 1 args
    | _ -> 100
*)

let string_of_html = Format.asprintf "%a" (Tyxml.Html.pp_elt ())
let fullname t = Format.asprintf "%a" Pretty.show_type_name_verbose t

let rec typ_of_odoc_typ otyp =
  match otyp with
  | Odoc_model.Lang.TypeExpr.Var str -> Db.Typepath.Poly str
  | Any -> Db.Typepath.Any
  | Arrow (_lbl, left, right) ->
      Db.Typepath.Arrow (typ_of_odoc_typ left, typ_of_odoc_typ right)
  | Constr (name, args) ->
      Db.Typepath.Constr (fullname name, List.map typ_of_odoc_typ args)
  | _ -> Db.Typepath.Unhandled

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
    else
      let chr = str.[i] in
      if (chr >= 'a' && chr <= 'z')
         || (chr >= '0' && chr <= '9')
         || chr = '_' || chr = '@'
      then Buffer.add_char buf chr
      else flush () ;
      go (i + 1)
  in
  go 0

let register_doc ~db elt doc_txt =
  with_tokenizer doc_txt @@ fun word -> Db.store_word db word elt

let register_full_name ~db name elt =
  let name = String.lowercase_ascii name in
  Db.store_word db name elt

let searchable_type_of_constructor args res =
  let open Odoc_model.Lang in
  match args with
  | TypeDecl.Constructor.Tuple args -> (
      match args with
      | _ :: _ :: _ -> TypeExpr.(Arrow (None, Tuple args, res))
      | [ arg ] -> TypeExpr.(Arrow (None, arg, res))
      | _ -> res)
  | TypeDecl.Constructor.Record fields ->
      List.fold_left
        (fun res field ->
          let open TypeDecl.Field in
          let field_name = Odoc_model.Paths.Identifier.name field.id in
          TypeExpr.Arrow (Some (Label field_name), field.type_, res))
        res fields

let searchable_type_of_record parent_type type_ =
  let open Odoc_model.Lang in
  TypeExpr.Arrow (None, parent_type, type_)

let convert_kind (Odoc_search.Entry.{ kind; _ } as entry) =
  let open Odoc_search.Entry in
  let type_path type_ =
    type_ |> typ_of_odoc_typ
    |> Db.Typepath.For_distance.of_typ ~ignore_any:false
  in
  match kind with
  | TypeDecl _ ->
      Elt.Kind.TypeDecl (Odoc_search.Html.typedecl_params_of_entry entry)
  | Module -> Elt.Kind.Module
  | Value { value = _; type_ } ->
      let paths = type_path type_ in
      Elt.Kind.val_ paths
  | Constructor { args; res } ->
      let searchable_type = searchable_type_of_constructor args res in
      let paths = type_path searchable_type in
      Elt.Kind.constructor paths
  | Field { mutable_ = _; parent_type; type_ } ->
      let paths = type_ |> searchable_type_of_record parent_type |> type_path in
      Elt.Kind.field paths
  | Doc _ -> Doc
  | Exception { args; res } ->
      let searchable_type = searchable_type_of_constructor args res in
      let paths = type_path searchable_type in
      Elt.Kind.exception_ paths
  | Class_type _ -> Class_type
  | Method _ -> Method
  | Class _ -> Class
  | TypeExtension _ -> TypeExtension
  | ExtensionConstructor { args; res } ->
      let searchable_type = searchable_type_of_constructor args res in
      let paths = type_path searchable_type in
      Elt.Kind.extension_constructor paths
  | ModuleType -> ModuleType

let register_type_expr ~db elt type_ =
  let type_paths =
    type_ |> typ_of_odoc_typ
    |> Db.Typepath.For_suffix_tree.of_typ ~ignore_any:false ~all_names:true
  in
  Db.store_type_paths db elt type_paths

let register_kind ~db ~type_search elt (kind : Odoc_search.Entry.kind) =
  let open Odoc_search.Entry in
  let open Odoc_model.Lang in
  if type_search
  then
    match kind with
    | TypeDecl _ -> ()
    | Module -> ()
    | Value { value = _; type_ } -> register_type_expr ~db elt type_
    | Doc _ -> ()
    | Class_type _ -> ()
    | Method _ -> ()
    | Class _ -> ()
    | TypeExtension _ -> ()
    | ModuleType -> ()
    | ExtensionConstructor { args; res }
    | Constructor { args; res }
    | Exception { args; res } ->
        let type_ = searchable_type_of_constructor args res in
        register_type_expr ~db elt type_
    | Field { mutable_ = _; parent_type; type_ } ->
        let type_ = TypeExpr.Arrow (None, parent_type, type_) in
        register_type_expr ~db elt type_

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

let register_entry ~db ~index_name ~type_search ~index_docstring
    (Odoc_search.Entry.{ id; doc; kind } as entry) =
  let open Odoc_search in
  let open Odoc_search.Entry in
  let is_type_extension =
    match kind with
    | TypeExtension _ -> true
    | _ -> false
  in
  if Odoc_model.Paths.Identifier.is_internal id || is_type_extension
  then ()
  else
    let full_name = id |> Pretty.fullname |> String.concat "." in
    let doc_txt = Text.of_doc doc in
    let doc_html =
      match doc_txt with
      | "" -> ""
      | _ -> doc |> Html.of_doc |> string_of_html
    in
    let kind' = convert_kind entry in
    let name =
      match kind with
      | Doc _ -> Pretty.prefixname id
      | _ -> full_name
    in
    let score = cost ~name ~kind:kind' ~doc_html in
    let rhs = Html.rhs_of_kind kind in
    let url = Html.url id in
    let url = Result.get_ok url in
    let is_from_module_type = is_from_module_type entry in
    let elt =
      Elt.v ~name ~kind:kind' ~rhs ~doc_html ~score ~url ~is_from_module_type ()
    in
    if index_docstring then register_doc ~db elt doc_txt ;
    (if index_name
     then
       match kind with
       | Doc _ -> ()
       | _ -> register_full_name ~db full_name elt) ;
    register_kind ~db ~type_search elt kind
