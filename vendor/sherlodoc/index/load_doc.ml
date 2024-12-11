module Entry = Db.Entry
module Db_common = Db
module ModuleName = Odoc_model.Names.ModuleName

let string_starts_with ~prefix str =
  let rec go i =
    if i >= String.length prefix then true else prefix.[i] = str.[i] && go (i + 1)
  in
  String.length prefix <= String.length str && go 0

let path_length str =
  let rec go i acc =
    if i >= String.length str
    then acc
    else go (i + 1) (if str.[i] = '.' then acc + 1 else acc)
  in
  go 0 0

let kind_cost = function
  | Entry.Kind.Constructor _ | Entry.Kind.Exception _ | Entry.Kind.Extension_constructor _
  | Entry.Kind.Field _ | Entry.Kind.Module | Entry.Kind.Type_decl _
  | Entry.Kind.Type_extension | Entry.Kind.Val _ ->
    0
  | _ -> 50

let rhs_cost = function
  | Some str -> String.length str
  | None -> 20

let cost_doc = function
  | Entry.Kind.Constructor _ | Entry.Kind.Exception _ | Entry.Kind.Extension_constructor _
  | Entry.Kind.Field _ | Entry.Kind.Module | Entry.Kind.Module_type
  | Entry.Kind.Type_decl _ | Entry.Kind.Type_extension ->
    0
  | _ -> 100

let cost ~name ~kind ~doc_html ~rhs ~cat ~favourite ~favoured_prefixes =
  String.length name
  + (5 * path_length name)
  + (if List.exists (fun prefix -> string_starts_with ~prefix name) favoured_prefixes
     then 0
     else 50)
  + (if favourite then 0 else 50)
  + rhs_cost rhs
  + kind_cost kind
  + (if cat = `definition then 0 else 100)
  + if doc_html <> "" then 0 else cost_doc kind

let string_of_html = Format.asprintf "%a" (Tyxml.Html.pp_elt ())

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
  with_tokenizer doc_txt @@ fun word -> Db_writer.store_word db word elt

let register_full_name ~db (elt : Db.Entry.t) =
  let name = String.lowercase_ascii elt.name in
  Db_writer.store_word db name elt

let searchable_type_of_constructor args res =
  let open Odoc_model.Lang in
  match args with
  | TypeDecl.Constructor.Tuple args -> begin
    match args with
    | _ :: _ :: _ -> TypeExpr.(Arrow (None, Tuple args, res))
    | [ arg ] -> TypeExpr.(Arrow (None, arg, res))
    | _ -> res
  end
  | TypeDecl.Constructor.Record fields ->
    List.fold_left
      (fun res field ->
        let open TypeDecl.Field in
        let field_name = Odoc_model.Paths.Identifier.name field.id in
        TypeExpr.Arrow (Some (Label field_name), field.type_, res))
      res
      fields

let searchable_type_of_record parent_type type_ =
  Odoc_model.Lang.TypeExpr.Arrow (None, parent_type, type_)

let convert_kind ~db (Odoc_search.Entry.{ kind; _ } as entry) =
  match kind with
  | TypeDecl _ -> Entry.Kind.Type_decl (Odoc_search.Html.typedecl_params_of_entry entry)
  | Value { value = _; type_ } ->
    let typ = Db_writer.type_of_odoc ~db type_ in
    Entry.Kind.Val typ
  | Constructor { args; res } ->
    let typ = searchable_type_of_constructor args res in
    let typ = Db_writer.type_of_odoc ~db typ in
    Entry.Kind.Constructor typ
  | ExtensionConstructor { args; res } ->
    let typ = searchable_type_of_constructor args res in
    let typ = Db_writer.type_of_odoc ~db typ in
    Entry.Kind.Extension_constructor typ
  | Exception { args; res } ->
    let typ = searchable_type_of_constructor args res in
    let typ = Db_writer.type_of_odoc ~db typ in
    Entry.Kind.Exception typ
  | Field { mutable_ = _; parent_type; type_ } ->
    let typ = searchable_type_of_record parent_type type_ in
    let typ = Db_writer.type_of_odoc ~db typ in
    Entry.Kind.Field typ
  | Doc _ -> Doc
  | Class_type _ -> Class_type
  | Method _ -> Method
  | Class _ -> Class
  | TypeExtension _ -> Type_extension
  | Module -> Entry.Kind.Module
  | ModuleType -> Module_type

let register_type_expr ~db elt typ =
  let type_polarities = Db.Type_polarity.of_typ ~any_is_poly:true typ in
  Db_writer.store_type_polarities db elt type_polarities

let register_kind ~db elt =
  let open Db.Entry in
  match Kind.get_type elt.kind with
  | None -> ()
  | Some typ -> register_type_expr ~db elt typ

let rec categorize id =
  let open Odoc_model.Paths in
  match id.Identifier.iv with
  | `CoreType _ | `CoreException _ | `Root _ | `Page _ | `LeafPage _ -> `definition
  | `ModuleType _ -> `declaration
  | `Parameter _ -> `ignore (* redundant with indexed signature *)
  | ( `InstanceVariable _ | `Method _ | `Field _ | `Result _ | `Label _ | `Type _
    | `Exception _ | `Class _ | `ClassType _ | `Value _ | `Constructor _ | `Extension _
    | `ExtensionDecl _ | `Module _ ) as x ->
    let parent = Identifier.label_parent { id with iv = x } in
    categorize (parent :> Identifier.Any.t)
  | `AssetFile _ | `SourceDir _ | `SourceLocationMod _ | `SourceLocation _ | `SourcePage _
  | `SourceLocationInternal _ ->
    `ignore (* unclear what to do with those *)

let categorize Odoc_search.Entry.{ id; _ } =
  match id.iv with
  | `ModuleType (parent, _) ->
    (* A module type itself is not *from* a module type, but it might be if one
       of its parents is a module type. *)
    categorize (parent :> Odoc_model.Paths.Identifier.Any.t)
  | _ -> categorize id

let register_entry
  ~db
  ~index_name
  ~type_search
  ~index_docstring
  ~favourite
  ~favoured_prefixes
  ~pkg
  ~cat
  (Odoc_search.Entry.{ id; doc; kind } as entry)
  =
  let module Sherlodoc_entry = Entry in
  let open Odoc_search in
  let name = String.concat "." (Odoc_model.Paths.Identifier.fullname id) in
  let doc_txt = Text.of_doc doc in
  let doc_html =
    match doc_txt with
    | "" -> ""
    | _ -> string_of_html (Html.of_doc doc)
  in
  let rhs = Html.rhs_of_kind kind in
  let kind = convert_kind ~db entry in
  let cost = cost ~name ~kind ~doc_html ~rhs ~cat ~favourite ~favoured_prefixes in
  let url = Result.get_ok (Html.url id) in
  let elt = Sherlodoc_entry.v ~name ~kind ~rhs ~doc_html ~cost ~url ~pkg () in
  if index_docstring then register_doc ~db elt doc_txt ;
  if index_name && kind <> Doc then register_full_name ~db elt ;
  if type_search then register_kind ~db elt

let register_entry
  ~db
  ~index_name
  ~type_search
  ~index_docstring
  ~favourite
  ~favoured_prefixes
  ~pkg
  (Odoc_search.Entry.{ id; kind; _ } as entry)
  =
  let cat = categorize entry in
  let is_pure_documentation =
    match kind with
    | Doc _ -> true
    | _ -> false
  in
  if is_pure_documentation || cat = `ignore || Odoc_model.Paths.Identifier.is_internal id
  then ()
  else
    register_entry
      ~db
      ~index_name
      ~type_search
      ~index_docstring
      ~favourite
      ~favoured_prefixes
      ~pkg
      ~cat
      entry
