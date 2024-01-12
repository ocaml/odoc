module Entry = Db.Entry

type query =
  { name : string list
  ; type_paths : Type_distance.Type_path.t option
  }

let of_query { Query_parser.name; typ } =
  let type_paths =
    match typ with
    | `typ t -> Some (Type_distance.Type_path.of_typ ~ignore_any:true t)
    | _ -> None
  in
  { name; type_paths }

module Reasoning = struct
  (** The [Reasoning] module contains a representation that include every reason
      for which a search entry would be ranked higher or lower. It does not
      decide which reason is more important. *)

  module Name_match = struct
    (** [Name_match.t] represents how good of a match there is between the query
        and the name of an entry. *)
    type t =
      | DotSuffix
      | PrefixSuffix
      | SubDot
      | SubUnderscore
      | Sub
      | Lowercase
      | Doc

    let with_word query_word name =
      let low_query_word = String.lowercase_ascii query_word in
      let has_case = low_query_word <> query_word in
      let name = if not has_case then String.lowercase_ascii name else name in
      if String.equal query_word name
         || Name_cost.ends_with ~suffix:("." ^ query_word) name
      then DotSuffix
      else if Name_cost.starts_with ~prefix:query_word name
              || Name_cost.ends_with ~suffix:query_word name
      then PrefixSuffix
      else if Name_cost.is_substring ~sub:("(" ^ query_word) name
              || Name_cost.is_substring ~sub:(query_word ^ ")") name
      then PrefixSuffix
      else if Name_cost.is_substring ~sub:("." ^ query_word) name
              || Name_cost.is_substring ~sub:(query_word ^ ".") name
      then SubDot
      else if Name_cost.is_substring ~sub:("_" ^ query_word) name
              || Name_cost.is_substring ~sub:(query_word ^ "_") name
      then SubUnderscore
      else if Name_cost.is_substring ~sub:query_word name
      then Sub
      else if has_case
              && Name_cost.is_substring ~sub:low_query_word (String.lowercase_ascii name)
      then Lowercase
      else (* Matches only in the docstring are always worse *) Doc

    let with_words query_words entry =
      match entry.Entry.kind with
      | Entry.Kind.Doc -> List.map (fun _ : t -> Doc) query_words
      | _ -> List.map (fun word -> with_word word entry.Entry.name) query_words
  end

  type t =
    { is_stdlib : bool
    ; name_length : int
    ; has_doc : bool
    ; name_matches : Name_match.t list
    ; type_distance : int option
    ; type_in_query : bool
    ; type_in_entry : bool
    ; kind : Entry.Kind.t
    ; is_from_module_type : bool
    }

  let type_distance query_type entry =
    match query_type, Entry.Kind.get_type entry.Entry.kind with
    | Some query_paths, Some entry_type ->
      Some (Type_distance.v ~query_paths ~entry:entry_type)
    | _ -> None

  let type_in_query = function
    | Some _ -> true
    | _ -> false

  let type_in_entry entry =
    let open Entry in
    match Entry.Kind.get_type entry.kind with
    | Some _ -> true
    | None -> false

  let is_stdlib entry =
    let open Entry in
    Name_cost.starts_with ~prefix:"Stdlib." entry.name

  let name_length entry = String.length entry.Entry.name
  let is_from_module_type entry = entry.Entry.is_from_module_type
  let has_doc e = e.Entry.doc_html <> ""

  (** Compute the reasoning for the cost of an entry *)
  let v { name = query_words; type_paths = query_type } entry =
    { is_stdlib = is_stdlib entry
    ; has_doc = has_doc entry
    ; name_matches = Name_match.with_words query_words entry
    ; type_distance = type_distance query_type entry
    ; type_in_entry = type_in_entry entry
    ; type_in_query = type_in_query query_type
    ; kind = entry.kind
    ; name_length = name_length entry
    ; is_from_module_type = is_from_module_type entry
    }
end

(** [cost_of_reasoning r] is the cost of a entry according to the reasons
    contained in [r]. *)
let cost_of_reasoning
  Reasoning.
    { is_stdlib
    ; has_doc
    ; name_matches
    ; type_distance
    ; type_in_entry
    ; type_in_query
    ; kind
    ; name_length
    ; is_from_module_type
    }
  =
  let ignore_no_doc =
    match kind with
    | Module | Module_type -> true
    | _ -> false
  in
  let kind =
    match kind with
    | Val _ | Module | Module_type | Constructor _ | Field _ | Type_decl _ -> 0
    | Exception _ -> 30
    | Class_type | Class | Type_extension -> 40
    | Extension_constructor _ | Method | Doc -> 50
  in
  let name_matches =
    let open Reasoning.Name_match in
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
    if type_in_entry && type_in_query
    then Option.get type_distance
    else if type_in_entry
    then 0
    else if type_in_query
    then
      (* If query request a type, elements which do not have one should never
         appear. *)
      assert false
    else 0
  in
  let is_from_module_type_cost = if is_from_module_type then 400 else 0 in
  (if is_stdlib then 0 else 100)
  + (if has_doc || ignore_no_doc then 0 else 100)
  + name_matches
  + type_cost
  + kind
  + name_length
  + is_from_module_type_cost

let cost_of_entry query entry = cost_of_reasoning (Reasoning.v query entry)
