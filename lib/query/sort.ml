open Common
module Elt = Db.Elt

module Type_distance = struct
  let distance xs ys =
    let len_xs = List.length xs in
    let len_ys = List.length ys in
    let cache = Array.make_matrix (1 + len_xs) (1 + len_ys) (-1) in
    let rec memo i j xs ys =
      let r = cache.(i).(j) in
      if r >= 0
      then r
      else begin
        let r = go i j xs ys in
        cache.(i).(j) <- r ;
        r
      end
    and go i j xs ys =
      match xs, ys with
      | [], _ -> 0
      | [ "_" ], _ -> 0
      | _, [] -> List.length xs
      | x :: xs, y :: ys when String.ends_with ~suffix:x y ->
          memo (i + 1) (j + 1) xs ys
      | _, "->1" :: ys -> memo i (j + 1) xs ys
      | "->1" :: xs, _ -> 1 + memo (i + 1) j xs ys
      | _ :: xs', _ :: ys' ->
          7
          + min
              (memo (i + 1) (j + 1) xs' ys')
              (min (memo (i + 1) j xs' ys) (memo i (j + 1) xs ys'))
    in
    go 0 0 xs ys

  let minimize = function
    | [] -> 0
    | arr ->
        let used = Array.make (List.length (List.hd arr)) false in
        let arr =
          Array.map (fun lst ->
              let lst = (1, None) :: List.mapi (fun i x -> x, Some i) lst in
              List.sort Stdlib.compare lst)
          @@ Array.of_list arr
        in
        Array.sort (fun xs ys -> Stdlib.compare xs ys) arr ;
        let heuristics = Array.make (Array.length arr + 1) 0 in
        for i = Array.length heuristics - 2 downto 0 do
          let best = fst (List.hd arr.(i)) in
          heuristics.(i) <- heuristics.(i + 1) + best
        done ;
        let best = ref 1000 in
        let limit = ref 0 in
        let rec go rem acc i =
          incr limit ;
          if !limit > 10_000
          then false
          else if rem <= 0
          then begin
            let score = acc + (1 * (Array.length arr - i)) in
            best := min score !best ;
            true
          end
          else if i >= Array.length arr
          then begin
            best := min !best (acc + (100 * rem)) ;
            true
          end
          else if acc + heuristics.(i) >= !best
          then true
          else
            let rec find = function
              | [] -> true
              | (cost, j) :: rest ->
                  let ok =
                    match j with
                    | None ->
                        go rem
                          (acc + cost
                          + if rem > Array.length arr - i then 100 else 0)
                          (i + 1)
                    | Some j ->
                        if used.(j)
                        then true
                        else begin
                          used.(j) <- true ;
                          let ok = go (rem - 1) (acc + cost) (i + 1) in
                          used.(j) <- false ;
                          ok
                        end
                  in
                  if ok then find rest else false
            in
            find arr.(i)
        in
        let _ = go (Array.length used) 0 0 in
        !best

  let v query_type paths =
    match paths, query_type with
    | _, [] | [], _ -> 0
    | _ ->
        let arr =
          List.map
            (fun p ->
              let p = List.rev p in
              List.map (fun q -> distance (List.rev q) p) query_type)
            paths
        in
        minimize arr
end

module Reasoning = struct
  module Name_match = struct
    type t =
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
      if String.starts_with ~prefix:query_word name
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
      else if String.lowercase_ascii query_word = String.lowercase_ascii name
      then Lowercase
      else (* Matches only in the docstring are always worse *) Doc

    let with_words query_words elt =
      match elt.Elt.kind with
      | Elt.Doc -> List.map (fun _ : t -> Doc) query_words
      | _ -> List.map (fun word -> with_word word elt.Elt.name) query_words

    let compare nm nm' =
      let to_int nm =
        match nm with
        | PrefixSuffix -> 0
        | SubDot -> 1
        | SubUnderscore -> 2
        | Sub -> 3
        | Lowercase -> 4
        | Doc -> 5
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
    | FunctorParameter
    | ModuleSubstitution
    | ModuleTypeSubstitution
    | InstanceVariable
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
    }

  let type_distance query_type elt =
    let open Elt in
    match query_type, elt.kind with
    | [], _ -> None
    | ( _
      , ( Elt.Constructor { type_paths; _ }
        | Elt.Field { type_paths; _ }
        | Elt.Val { type_paths; _ } ) ) ->
        Some (Type_distance.v query_type type_paths)
    | _ -> None

  let type_in_query query_type = query_type <> []

  let type_in_elt elt =
    let open Elt in
    match elt.kind with
    | Constructor _ | Field _ | Val _ -> true
    | _ -> false

  let is_stdlib elt =
    let open Elt in
    String.starts_with ~prefix:"Stdlib." elt.name

  let has_doc elt =
    let open Elt in
    elt.doc.txt <> ""

  let kind elt =
    match elt.Elt.kind with
    | Elt.Doc -> Doc
    | Elt.TypeDecl _ -> TypeDecl
    | Elt.Module -> Module
    | Elt.Exception -> Exception
    | Elt.Class_type -> Class_type
    | Elt.Method -> Method
    | Elt.Class -> Class
    | Elt.TypeExtension -> TypeExtension
    | Elt.ExtensionConstructor -> ExtensionConstructor
    | Elt.ModuleType -> ModuleType
    | Elt.Constructor _ -> Constructor
    | Elt.Field _ -> Field
    | Elt.FunctorParameter -> FunctorParameter
    | Elt.ModuleSubstitution -> ModuleSubstitution
    | Elt.ModuleTypeSubstitution -> ModuleTypeSubstitution
    | Elt.InstanceVariable -> InstanceVariable
    | Elt.Val _ -> Val

  let name_length elt = String.length elt.Elt.name

  let v query_words query_type elt =
    let is_stdlib = is_stdlib elt in
    let has_doc = has_doc elt in
    let name_matches = Name_match.with_words query_words elt in
    let kind = kind elt in
    let type_distance = type_distance query_type elt in
    let type_in_elt = type_in_elt elt in
    let type_in_query = type_in_query query_type in
    let name_length = name_length elt in
    { is_stdlib
    ; has_doc
    ; name_matches
    ; type_distance
    ; type_in_elt
    ; type_in_query
    ; kind
    ; name_length
    }

  let compare_is_stblib b1 b2 = if b1 && b2 then 0 else if b1 then -1 else 1
  let compare_has_doc b1 b2 = if b1 && b2 then 0 else if b1 then -1 else 1

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
      | FunctorParameter -> 6
      | Method -> 5
      | ModuleSubstitution -> 5
      | ModuleTypeSubstitution -> 5
      | InstanceVariable -> 6
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
      } =
    let kind =
      match kind with
      | Val | Module | Constructor | Field | TypeDecl -> 0
      | ModuleType -> 20
      | Exception -> 30
      | Class_type | Class | TypeExtension -> 40
      | ExtensionConstructor | Method | ModuleSubstitution
      | ModuleTypeSubstitution | Doc ->
          50
      | FunctorParameter | InstanceVariable -> 60
    in
    let name_matches =
      let open Name_match in
      name_matches
      |> List.map (function
           | PrefixSuffix -> 0
           | SubDot -> 1
           | SubUnderscore -> 2
           | Sub -> 3
           | Lowercase -> 4
           | Doc -> 1000)
      |> List.fold_left ( + ) 0
    in
    let type_cost =
      if type_in_elt && type_in_query
      then Option.get type_distance
      else if type_in_elt
      then 0
      else
        (* If query request a type, elements which do not have one are not to be
           placed high. *)
        10000
    in
    (if is_stdlib then 0 else 100)
    + (if has_doc then 0 else 500)
    + name_matches + type_cost + kind + name_length

  let compare r r' = Int.compare (score r) (score r')
end

let list query_name query_type results =
  let open Reasoning in
  let f = v query_name query_type in
  List.sort_map ~f ~compare results
