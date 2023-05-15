module Db_common = Db
open Db.Caches

module Make (Storage : Db.Storage.S) = struct
  module Types = Db.Types
  module Db = Db.Make (Storage)
  module ModuleName = Odoc_model.Names.ModuleName

  let clear () = Cache.clear ()

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

  let rec paths ~prefix ~sgn = function
    | Odoc_model.Lang.TypeExpr.Var _ ->
        let poly = Cache.memo "POLY" in
        [ poly :: Cache.memo (Types.string_of_sgn sgn) :: prefix ]
    | Any ->
        let poly = Cache.memo "POLY" in
        [ poly :: Cache.memo (Types.string_of_sgn sgn) :: prefix ]
    | Arrow (_, a, b) ->
        let prefix_left = Cache.memo "->0" :: prefix in
        let prefix_right = Cache.memo "->1" :: prefix in
        List.rev_append
          (paths ~prefix:prefix_left ~sgn:(Types.sgn_not sgn) a)
          (paths ~prefix:prefix_right ~sgn b)
    | Constr (name, args) ->
        let name = fullname name in
        let prefix =
          Cache.memo name :: Cache.memo (Types.string_of_sgn sgn) :: prefix
        in
        begin
          match args with
          | [] -> [ prefix ]
          | _ ->
              rev_concat
              @@ List.mapi
                   (fun i arg ->
                     let prefix = Cache.memo (string_of_int i) :: prefix in
                     paths ~prefix ~sgn arg)
                   args
        end
    | Tuple args ->
        rev_concat
        @@ List.mapi (fun i arg ->
               let prefix = Cache.memo (string_of_int i ^ "*") :: prefix in
               paths ~prefix ~sgn arg)
        @@ args
    | _ -> []

  let rec type_paths ~prefix ~sgn = function
    | Odoc_model.Lang.TypeExpr.Var _ ->
        [ "POLY" :: Types.string_of_sgn sgn :: prefix ]
    | Any -> [ "POLY" :: Types.string_of_sgn sgn :: prefix ]
    | Arrow (_lbl, a, b) ->
        List.rev_append
          (type_paths ~prefix ~sgn:(Types.sgn_not sgn) a)
          (type_paths ~prefix ~sgn b)
    | Constr (name, args) ->
        rev_concat
        @@ List.map (fun name ->
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
                            type_paths ~prefix ~sgn arg)
                          args
               end)
        @@ all_type_names name
    | Tuple args -> rev_concat @@ List.map (type_paths ~prefix ~sgn) @@ args
    | _ -> []

  let register_doc elt doc_txt =
    let doc_words = String.split_on_char ' ' doc_txt in
    List.iter (fun word -> Db.store_word word elt) doc_words

  let register_full_name name elt =
    let name = String.lowercase_ascii name in
    Db.store_word name elt

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

  let display_type_expr type_ =
    let open Odoc_search in
    let html = type_ |> Render.html_of_type |> string_of_html in
    let txt = Render.text_of_type type_ in
    Db_common.Elt.{ html; txt }

  let generic_cost ~ignore_no_doc full_name doc =
    String.length full_name
    (* + (5 * List.length path) TODO : restore depth based ordering *)
    + (if ignore_no_doc
       then 0
       else
         match Db_common.Elt.(doc.txt) with
         | "" -> 1000
         | _ -> 0)
    + if String.starts_with ~prefix:"Stdlib." full_name then -100 else 0

  let type_cost type_ =
    String.length (display_type_expr type_).txt + type_size type_

  let kind_cost (kind : Odoc_search.Index_db.kind) =
    let open Odoc_search in
    let open Odoc_search.Index_db in
    match kind with
    | Constructor { args; res } ->
        type_cost (searchable_type_of_constructor args res)
    | Field { parent_type; type_; _ } ->
        type_cost (searchable_type_of_record parent_type type_)
    | Value { value = _; type_ } -> type_cost type_
    | Doc _ -> 400
    | TypeDecl _ | Module | Exception _ | Class_type _ | Method _ | Class _
    | TypeExtension _ | ExtensionConstructor _ | ModuleType | FunctorParameter
    | ModuleSubstitution _ | ModuleTypeSubstitution | InstanceVariable _ ->
        200

  let convert_kind (kind : Odoc_search.Index_db.kind) =
    let open Odoc_search in
    let open Odoc_search.Index_db in
    match kind with
    | TypeDecl typedecl ->
        let html = typedecl |> Render.html_of_typedecl |> string_of_html in
        Db_common.Elt.TypeDecl { html }
    | Module -> Db_common.Elt.ModuleType
    | Value { value = _; type_ } ->
        let paths = paths ~prefix:[] ~sgn:Pos type_ in
        let type_ = display_type_expr type_ in
        Val { type_; type_paths = paths }
    | Constructor { args; res } ->
        let type_ = searchable_type_of_constructor args res in
        let type_paths = paths ~prefix:[] ~sgn:Pos type_ in
        let type_ = display_type_expr type_ in
        Constructor { type_; type_paths }
    | Field { mutable_ = _; parent_type; type_ } ->
        let type_ = searchable_type_of_record parent_type type_ in
        let type_paths = paths ~prefix:[] ~sgn:Pos type_ in
        let type_ = display_type_expr type_ in
        Field { type_; type_paths }
    | Doc _ -> Doc
    | Exception _ -> Exception
    | Class_type _ -> Class_type
    | Method _ -> Method
    | Class _ -> Class
    | TypeExtension _ -> TypeExtension
    | ExtensionConstructor _ -> ExtensionConstructor
    | ModuleType -> ModuleType
    | FunctorParameter -> FunctorParameter
    | ModuleSubstitution _ -> ModuleSubstitution
    | ModuleTypeSubstitution -> ModuleTypeSubstitution
    | InstanceVariable _ -> InstanceVariable

  let register_type_expr elt type_ =
    let type_paths = type_paths ~prefix:[] ~sgn:Pos type_ in
    (* let str = String.concat "|" (List.concat_map (fun li -> ";" :: li)  type_paths) in
        print_endline str; *)
    Db.store_type elt
      (List.map
         (fun xs ->
           let xs = List.concat_map Db_common.list_of_string xs in
           Cache_list.memo xs)
         type_paths)

  let register_kind elt (kind : Odoc_search.Index_db.kind) =
    let open Odoc_search.Index_db in
    let open Odoc_model.Lang in
    match kind with
    | TypeDecl _ -> ()
    | Module -> ()
    | Value { value = _; type_ } -> register_type_expr elt type_
    | Doc _ -> ()
    | Exception _ -> ()
    | Class_type _ -> ()
    | Method _ -> ()
    | Class _ -> ()
    | TypeExtension _ -> ()
    | ExtensionConstructor _ -> ()
    | ModuleType -> ()
    | Constructor { args; res } ->
        let type_ = searchable_type_of_constructor args res in
        register_type_expr elt type_
    | Field { mutable_ = _; parent_type; type_ } ->
        let type_ = TypeExpr.Arrow (None, parent_type, type_) in
        register_type_expr elt type_
    | FunctorParameter -> ()
    | ModuleSubstitution _ -> ()
    | ModuleTypeSubstitution -> ()
    | InstanceVariable _ -> ()

  let register_entry
      Odoc_search.Index_db.
        { id : Odoc_model.Paths.Identifier.Any.t
        ; doc : Odoc_model.Comment.docs
        ; kind : kind
        } =
    let open Odoc_search in
    let open Odoc_search.Index_db in
    let full_name =
      id |> Odoc_model.Paths.Identifier.fullname |> String.concat "."
    in
    let url = Render.url id in
    let doc =
      let html = doc |> Render.html_of_doc |> string_of_html
      and txt = Render.text_of_doc doc in
      Db_common.Elt.{ html; txt }
    in
    let kind' = convert_kind kind in
    let ignore_no_doc =
      match kind with
      | Module | ModuleType -> true
      | _ -> false
    in
    let cost = generic_cost ~ignore_no_doc full_name doc + kind_cost kind in
    let name =
      match kind with
      | Doc _ -> Odoc_model.Paths.Identifier.prefixname id
      | _ -> full_name
    in
    let elt =
      Db_common.Elt.{ name; url; kind = kind'; cost; doc; pkg = None }
    in
    register_doc elt doc.txt ;
    (match kind with
    | Doc _ -> ()
    | _ -> register_full_name full_name elt) ;
    register_kind elt kind

  module Resolver = Odoc_odoc.Resolver

  let run ~index = List.iter register_entry index
end
