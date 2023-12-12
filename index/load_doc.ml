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

let rev_concat lst =
  List.fold_left (fun acc xs -> List.rev_append xs acc) [] lst

let rec tails = function
  | [] -> []
  | _ :: xs as lst -> lst :: tails xs

let string_of_html = Format.asprintf "%a" (Tyxml.Html.pp_elt ())
let fullname t = Format.asprintf "%a" Pretty.show_type_name_verbose t

let all_type_names t =
  let fullname = fullname t in
  tails (String.split_on_char '.' fullname)

let rec type_distance_paths ~prefix ~sgn t =
  match t with
  | Odoc_model.Lang.TypeExpr.Var _ ->
      let poly = "POLY" in
      [ poly :: Types.string_of_sgn sgn :: prefix ]
  | Any ->
      let poly = "POLY" in
      [ poly :: Types.string_of_sgn sgn :: prefix ]
  | Arrow (_, a, b) ->
      let prefix_left = "->0" :: prefix in
      let prefix_right = "->1" :: prefix in
      List.rev_append
        (type_distance_paths ~prefix:prefix_left ~sgn:(Types.sgn_not sgn) a)
        (type_distance_paths ~prefix:prefix_right ~sgn b)
  | Constr (name, args) ->
      let name = fullname name in
      let prefix = name :: Types.string_of_sgn sgn :: prefix in
      begin
        match args with
        | [] -> [ prefix ]
        | _ ->
            rev_concat
            @@ List.mapi
                 (fun i arg ->
                   let prefix = string_of_int i :: prefix in
                   type_distance_paths ~prefix ~sgn arg)
                 args
      end
  | Tuple args ->
      rev_concat
      @@ List.mapi (fun i arg ->
             let prefix = (string_of_int i ^ "*") :: prefix in
             type_distance_paths ~prefix ~sgn arg)
      @@ args
  | _ -> []

let hcons_tbl = Hashtbl.create 16
let uid_generator = ref 0

let rec hcons = function
  | [] -> -1, []
  | x :: xs -> (
      let uid_xs, xs = hcons xs in
      match Hashtbl.find hcons_tbl (uid_xs, x) with
      | xxs -> xxs
      | exception Not_found ->
          let uid = !uid_generator in
          uid_generator := uid + 1 ;
          let result = uid, x :: xs in
          Hashtbl.add hcons_tbl (uid_xs, x) result ;
          result)

(** [type_distance_paths ~prefix ~sgn t] is a [string list list] representing
    the type [t]. It allows to compute the distance between two types. It is
    stored in the database to sort results once they are obtained. *)
let type_distance_paths typ =
  List.map
    (fun xs ->
      let _, xs = hcons xs in
      xs)
    (type_distance_paths ~prefix:[] ~sgn:Pos typ)

let rec suffix_tree_type_paths ~prefix ~sgn = function
  | Odoc_model.Lang.TypeExpr.Var _ ->
      [ "POLY" :: Types.string_of_sgn sgn :: prefix ]
  | Any -> [ "POLY" :: Types.string_of_sgn sgn :: prefix ]
  | Arrow (_lbl, a, b) ->
      List.rev_append
        (suffix_tree_type_paths ~prefix ~sgn:(Types.sgn_not sgn) a)
        (suffix_tree_type_paths ~prefix ~sgn b)
  | Constr (name, args) ->
      name |> all_type_names
      |> List.map (fun name ->
             let name = String.concat "." name in
             let prefix = name :: Types.string_of_sgn sgn :: prefix in
             begin
               match args with
               | [] -> [ prefix ]
               | _ ->
                   rev_concat
                   @@ List.mapi
                        (fun i arg ->
                          let prefix = string_of_int i :: prefix in
                          suffix_tree_type_paths ~prefix ~sgn arg)
                        args
             end)
      |> rev_concat
  | Tuple args ->
      rev_concat @@ List.map (suffix_tree_type_paths ~prefix ~sgn) @@ args
  | _ -> []

(** [suffix_tree_type_paths ~prefix ~sgn t] is a representation of [t] that
    encodes the polarity of the elements of the type : in [string -> int] [int]
    is positive and [string] negative.
    It is registered in the database and search-base type uses this to obtain
    results that fit the type asked for by the user. *)
let suffix_tree_type_paths t = suffix_tree_type_paths ~prefix:[] ~sgn:Pos t

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
  match kind with
  | TypeDecl _ ->
      Elt.Kind.TypeDecl (Odoc_search.Html.typedecl_params_of_entry entry)
  | Module -> Elt.Kind.Module
  | Value { value = _; type_ } ->
      let paths = type_distance_paths type_ in
      Elt.Kind.val_ paths
  | Constructor { args; res } ->
      let searchable_type = searchable_type_of_constructor args res in
      let paths = type_distance_paths searchable_type in
      Elt.Kind.constructor paths
  | Field { mutable_ = _; parent_type; type_ } ->
      let paths =
        type_ |> searchable_type_of_record parent_type |> type_distance_paths
      in
      Elt.Kind.field paths
  | Doc _ -> Doc
  | Exception { args; res } ->
      let searchable_type = searchable_type_of_constructor args res in
      let paths = type_distance_paths searchable_type in
      Elt.Kind.exception_ paths
  | Class_type _ -> Class_type
  | Method _ -> Method
  | Class _ -> Class
  | TypeExtension _ -> TypeExtension
  | ExtensionConstructor { args; res } ->
      let searchable_type = searchable_type_of_constructor args res in
      let paths = type_distance_paths searchable_type in
      Elt.Kind.extension_constructor paths
  | ModuleType -> ModuleType

let register_type_expr ~db elt type_ =
  let type_paths = suffix_tree_type_paths type_ in
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
