module Elt = Db.Elt

module Reasoning = struct
  module Name_match = struct
    type t =
      | DotSuffix
      | PrefixSuffix
      | SubDot
      | SubUnderscore
      | Sub
      | Lowercase
      | Doc

    let is_substring ~sub s =
      let re = Re.(compile (seq [ rep any; str sub ])) in
      Re.execp re s

    let with_word query_word name =
      let low_query_word = String.lowercase_ascii query_word in
      let has_case = low_query_word <> query_word in
      let name = if not has_case then String.lowercase_ascii name else name in
      if String.equal query_word name
         || String.ends_with ~suffix:("." ^ query_word) name
      then DotSuffix
      else if String.starts_with ~prefix:query_word name
              || String.ends_with ~suffix:query_word name
      then PrefixSuffix
      else if is_substring ~sub:("(" ^ query_word) name
              || is_substring ~sub:(query_word ^ ")") name
      then PrefixSuffix
      else if is_substring ~sub:("." ^ query_word) name
              || is_substring ~sub:(query_word ^ ".") name
      then SubDot
      else if is_substring ~sub:("_" ^ query_word) name
              || is_substring ~sub:(query_word ^ "_") name
      then SubUnderscore
      else if is_substring ~sub:query_word name
      then Sub
      else if has_case
              && is_substring ~sub:low_query_word (String.lowercase_ascii name)
      then Lowercase
      else (* Matches only in the docstring are always worse *) Doc

    let with_words query_words elt =
      match elt.Elt.kind with
      | Elt.Kind.Doc -> List.map (fun _ : t -> Doc) query_words
      | _ -> List.map (fun word -> with_word word elt.Elt.name) query_words

    let compare nm nm' =
      let to_int nm =
        match nm with
        | DotSuffix -> 0
        | PrefixSuffix -> 1
        | SubDot -> 2
        | SubUnderscore -> 3
        | Sub -> 4
        | Lowercase -> 5
        | Doc -> 6
      in
      Int.compare (to_int nm) (to_int nm')
  end

  type kind =
    | Doc
    | TypeDecl
    | Module
    | Exception
    | Class_type
    | Method
    | Class
    | TypeExtension
    | ExtensionConstructor
    | ModuleType
    | Constructor
    | Field
    | Val

  type t =
    { is_stdlib : bool
    ; name_length : int
    ; has_doc : bool
    ; name_matches : Name_match.t list
    ; type_distance : int option
    ; type_in_query : bool
    ; type_in_elt : bool
    ; kind : kind
    ; is_from_module_type : bool
    }

  let type_distance query_type elt =
    let open Elt in
    match query_type, elt.kind with
    | None, _ -> None
    | ( Some query_type
      , Elt.Kind.(
          ( ExtensionConstructor eltype
          | Constructor eltype
          | Field eltype
          | Val eltype
          | Exception eltype )) ) ->
        Some (Type_distance.v ~query:query_type ~element:eltype)
    | ( _
      , ( Doc | TypeDecl _ | Module | Class_type | Method | Class
        | TypeExtension | ModuleType ) ) ->
        None

  let type_in_query query_type = Option.is_some query_type

  let type_in_elt elt =
    let open Elt in
    match elt.kind with
    | ExtensionConstructor _ | Constructor _ | Field _ | Val _ | Exception _ ->
        true
    | Doc | TypeDecl _ | Module | Class_type | Method | Class | TypeExtension
    | ModuleType ->
        false

  let is_stdlib elt =
    let open Elt in
    String.starts_with ~prefix:"Stdlib." elt.name

  let kind elt =
    match elt.Elt.kind with
    | Elt.Kind.Doc -> Doc
    | Elt.Kind.TypeDecl _ -> TypeDecl
    | Elt.Kind.Module -> Module
    | Elt.Kind.Exception _ -> Exception
    | Elt.Kind.Class_type -> Class_type
    | Elt.Kind.Method -> Method
    | Elt.Kind.Class -> Class
    | Elt.Kind.TypeExtension -> TypeExtension
    | Elt.Kind.ExtensionConstructor _ -> ExtensionConstructor
    | Elt.Kind.ModuleType -> ModuleType
    | Elt.Kind.Constructor _ -> Constructor
    | Elt.Kind.Field _ -> Field
    | Elt.Kind.Val _ -> Val

  let name_length elt = String.length elt.Elt.name
  let is_from_module_type elt = elt.Elt.is_from_module_type

  let v query_words query_type elt =
    let is_stdlib = is_stdlib elt in
    let has_doc = elt.Elt.doc_html <> "" in
    let name_matches = Name_match.with_words query_words elt in
    let kind = kind elt in
    let type_distance = type_distance query_type elt in
    let type_in_elt = type_in_elt elt in
    let type_in_query = type_in_query query_type in
    let name_length = name_length elt in
    let is_from_module_type = is_from_module_type elt in
    { is_stdlib
    ; has_doc
    ; name_matches
    ; type_distance
    ; type_in_elt
    ; type_in_query
    ; kind
    ; name_length
    ; is_from_module_type
    }

  let compare_kind k k' =
    let to_int = function
      | Val -> 0
      | Module -> 0
      | Doc -> 5
      | Constructor -> 1
      | Field -> 1
      | TypeDecl -> 1
      | ModuleType -> 2
      | Exception -> 3
      | Class_type -> 4
      | Class -> 4
      | TypeExtension -> 4
      | ExtensionConstructor -> 5
      | Method -> 5
    in
    Int.compare (to_int k) (to_int k')

  let score
      { is_stdlib
      ; has_doc
      ; name_matches
      ; type_distance
      ; type_in_elt
      ; type_in_query
      ; kind
      ; name_length
      ; is_from_module_type
      } =
    let ignore_no_doc =
      match kind with
      | Module | ModuleType -> true
      | _ -> false
    in
    let kind =
      match kind with
      | Val | Module | ModuleType | Constructor | Field | TypeDecl -> 0
      | Exception -> 30
      | Class_type | Class | TypeExtension -> 40
      | ExtensionConstructor | Method | Doc -> 50
    in
    let name_matches =
      let open Name_match in
      name_matches
      |> List.map (function
           | DotSuffix -> 0
           | PrefixSuffix -> 103
           | SubDot -> 104
           | SubUnderscore -> 105
           | Sub -> 106
           | Lowercase -> 107
           | Doc -> 1000)
      |> List.fold_left ( + ) 0
    in

    let type_cost =
      if type_in_elt && type_in_query
      then Option.get type_distance
      else if type_in_elt
      then 0
      else if type_in_query
      then
        (* If query request a type, elements which do not have one should never
           appear. *)
        (* assert false *)
        0
      else 0
    in
    let is_from_module_type_cost = if is_from_module_type then 400 else 0 in
    (if is_stdlib then 0 else 100)
    + (if has_doc || ignore_no_doc then 0 else 100)
    + name_matches + type_cost + kind + name_length + is_from_module_type_cost

  let score ~query_name ~query_type elt = score (v query_name query_type elt)
end

let elt ~query_name ~query_type elt =
  Elt.{ elt with score = Reasoning.score ~query_name ~query_type elt }
