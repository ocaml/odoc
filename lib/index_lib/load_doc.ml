module Db_common = Db

module Make (Storage : Db.Storage.S) = struct
  module Types = Db.Types
  module Db = Db.Make (Storage)
  open Odoc_model
  module ModuleName = Odoc_model.Names.ModuleName

  module Cache = Cache.Make (struct
    type t = string

    let copy str = String.init (String.length str) (String.get str)
  end)

  module Cache_list = struct
    module H = Hashtbl.Make (struct
      type t = char list

      let equal = List.equal Char.equal
      let hash = Hashtbl.hash
    end)

    let cache = H.create 128

    let memo lst =
      let rec go lst =
        try H.find cache lst
        with Not_found ->
          let lst =
            match lst with
            | [] -> []
            | x :: xs -> x :: go xs
          in
          H.add cache lst lst ;
          lst
      in
      go lst
  end

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

  let fullname t =
    Pretty.fmt_to_string (fun h -> Pretty.show_type_name_verbose h t)

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

  let save_doc elt doc =
    let doc_words = Docstring.words_of_docs doc in
    List.iter (fun word -> Db.store_name (Cache_list.memo word) elt) doc_words

  let save_full_name path_list name elt =
    let my_full_name =
      List.rev_append (Db_common.list_of_string name) ('.' :: path_list)
    in
    let my_full_name = List.map Char.lowercase_ascii my_full_name in
    Db.store_name (Cache_list.memo my_full_name) elt

  let generic_cost ~ignore_no_doc full_name path str_doc =
    String.length full_name
    + (5 * List.length path)
    + (if ignore_no_doc
       then 0
       else
         match str_doc with
         | None -> 1000
         | _ -> 0)
    + if String.starts_with ~prefix:"Stdlib." full_name then -100 else 0

  let save_val ~pkg ~path_list ~path name type_ doc =
    let str_type =
      Format.kasprintf Cache.memo "%a%!"
        (Pretty.show_type
           ~path:(Pretty.fmt_to_string (fun h -> Pretty.pp_path h path))
           ~parens:false)
        type_
    in
    let full_name =
      Format.asprintf "%a%s%!" Pretty.pp_path path
        (Odoc_model.Names.ValueName.to_string name)
    in

    let str_doc = Option.map Cache.memo (Pretty.string_of_docs doc) in
    let cost =
      generic_cost ~ignore_no_doc:false full_name path str_doc
      + String.length str_type + type_size type_
    in
    let paths = paths ~prefix:[] ~sgn:Pos type_ in
    let elt =
      { Db_common.Elt.name = full_name
      ; kind = Db_common.Elt.Val { type_paths = paths; str_type }
      ; cost
      ; doc = str_doc
      ; pkg
      }
    in
    save_doc elt doc ;
    save_full_name path_list (Odoc_model.Names.ValueName.to_string name) elt ;
    let type_paths = type_paths ~prefix:[] ~sgn:Pos type_ in
    Db.store_all elt
      (List.map
         (fun xs ->
           let xs = List.concat_map Db_common.list_of_string xs in
           Cache_list.memo xs)
         type_paths)

  let save_named_elt ~pkg ~path_list ~path ~kind name doc =
    let full_name = Format.asprintf "%a%s%!" Pretty.pp_path path name in
    let str_doc = Option.map Cache.memo (Pretty.string_of_docs doc) in
    let ignore_no_doc =
      match kind with
      | Db_common.Elt.(Module | ModuleType) -> true
      | _ -> false
    in
    let cost = generic_cost ~ignore_no_doc full_name path str_doc in
    let elt =
      { Db_common.Elt.name = full_name; kind; cost; doc = str_doc; pkg }
    in
    save_doc elt doc ;
    save_full_name path_list name elt

  let rec item ~pkg ~path_list ~path =
    let open Odoc_model.Lang in
    function
    | Signature.Value { id = `Value (_, name); _ }
      when Odoc_model.Names.ValueName.is_internal name ->
        ()
    | Signature.Value { id = `Value (_, name); type_; doc; _ } ->
        save_val ~pkg ~path_list ~path name type_ doc
    | Module
        ( _
        , ({ id = `Module (_, name) | `Root (_, name)
           ; doc
           ; hidden =
               _ (* TODO : should hidden modules show up in search results ?*)
           ; _
           } as mdl) ) ->
        let name = Odoc_model.Names.ModuleName.to_string name in
        save_named_elt ~pkg ~path_list ~path ~kind:Module name doc ;
        let name = Paths.Identifier.name mdl.id in
        if name = "Stdlib" then () else module_items ~pkg ~path_list ~path mdl
    | Type (_, { id = `Type (_, name) | `CoreType name; doc; _ }) ->
        let name = Odoc_model.Names.TypeName.to_string name in
        save_named_elt ~pkg ~path_list ~path ~kind:Type name doc
    | Include icl -> items ~pkg ~path_list ~path icl.expansion.content.items
    | TypeSubstitution _ -> () (* type t = Foo.t = actual_definition *)
    | TypExt _ -> () (* type t = .. *)
    | Exception { id = `Exception (_, name) | `CoreException name; doc; _ } ->
        let name = Odoc_model.Names.ExceptionName.to_string name in
        save_named_elt ~pkg ~path_list ~path ~kind:Exception name doc
    | Class _ -> ()
    | ClassType _ -> ()
    | Comment _ -> ()
    | Open _ -> ()
    | ModuleType { id = `ModuleType (_, name); doc; _ } ->
        let name = Odoc_model.Names.ModuleTypeName.to_string name in
        save_named_elt ~pkg ~path_list ~path ~kind:ModuleType name doc
    | ModuleSubstitution _ -> ()
    | ModuleTypeSubstitution _ -> ()

  and items ~pkg ~path_list ~path item_list =
    List.iter (item ~pkg ~path_list ~path) item_list

  and module_items ~pkg ~path_list ~path mdl =
    let open Odoc_model.Lang.Module in
    let name = Paths.Identifier.name mdl.id in
    let path = name :: path in
    let path_list =
      List.rev_append (Db_common.list_of_string name) ('.' :: path_list)
    in
    match mdl.type_ with
    | ModuleType e -> module_type_expr ~pkg ~path_list ~path e
    | Alias (_, Some mdl) -> module_items_ty ~pkg ~path_list ~path mdl
    | Alias (_, None) -> ()

  and module_type_expr ~pkg ~path_list ~path = function
    | Signature sg -> items ~pkg ~path_list ~path sg.items
    | Functor (_, sg) -> module_type_expr ~pkg ~path_list ~path sg
    | With { w_expansion = Some sg; _ }
    | TypeOf { t_expansion = Some sg; _ }
    | Path { p_expansion = Some sg; _ } ->
        simple_expansion ~pkg ~path_list ~path sg
    | With _ -> ()
    | TypeOf _ -> ()
    | Path _ -> ()
    | _ -> .

  and simple_expansion ~pkg ~path_list ~path = function
    | Signature sg -> items ~pkg ~path_list ~path sg.items
    | Functor (_, sg) -> simple_expansion ~pkg ~path_list ~path sg

  and module_items_ty ~pkg ~path_list ~path = function
    | Functor (_, mdl) -> module_items_ty ~pkg ~path_list ~path mdl
    | Signature sg -> items ~pkg ~path_list ~path sg.items

  module Resolver = Odoc_odoc.Resolver

  let run ~odoc_directory (root_name, filename) =
    let ((package, version) as pkg) =
      match String.split_on_char '/' filename with
      | "." :: package :: version :: _ -> package, version
      | _ ->
          invalid_arg
            (Printf.sprintf "not a valid package/version? %S" filename)
    in
    Format.printf "%s %s => %s@." package version root_name ;
    let filename = Filename.concat odoc_directory filename in
    let fpath = Result.get_ok @@ Fpath.of_string filename in
    let t =
      match Odoc_odoc.Odoc_file.load fpath with
      | Ok { Odoc_odoc.Odoc_file.content = Unit_content t; _ } -> t
      | Ok { Odoc_odoc.Odoc_file.content = Page_content _; _ } ->
          failwith "page content"
      | Error (`Msg m) -> failwith ("ERROR:" ^ m)
    in
    let open Odoc_model.Lang.Compilation_unit in
    match t.content with
    | Pack _ -> ()
    | Module t ->
        let path = [ root_name ] in
        let path_list = List.rev (Db_common.list_of_string root_name) in
        items ~pkg ~path_list ~path t.Odoc_model.Lang.Signature.items
end
