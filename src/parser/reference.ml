module Error = Model.Error
module Location_ = Model.Location_
module Paths = Model.Paths

(* http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec359. *)
let match_ocamldoc_reference_kind _warnings s
    : (_ Paths.Reference.tag) option =
  match s with
  | Some "module" -> Some TModule
  | Some "modtype" -> Some TModuleType
  | Some "class" -> Some TClass
  | Some "classtype" -> Some TClassType
  | Some "val" -> Some TValue
  | Some "type" -> Some TType
  | Some "exception" -> Some TException
  | Some "attribute" -> None
  | Some "method" -> Some TMethod
  | Some "section" -> Some TLabel
  | Some "const" -> Some TConstructor
  | Some "recfield" -> Some TField
  | _ -> None

let match_extra_odoc_reference_kind _warnings s
    : (_ Paths.Reference.tag) option =
  match s with
  | Some "class-type" -> Some TClassType
  | Some "constructor" -> Some TConstructor
  | Some "exn" -> Some TException
  | Some "extension" -> Some TExtension
  | Some "field" -> Some TField
  | Some "instance-variable" -> Some TInstanceVariable
  | Some "label" -> Some TLabel
  | Some "module-type" -> Some TModuleType
  | Some "page" -> Some TPage
  | Some "value" -> Some TValue
  | _ -> None

(* Ideally, the parser would call this on every reference kind annotation during
   tokenization. However, that constraints the phantom tag type to be the same
   for all tokens in the resulting token list (because lists are homogeneous).
   So, the parser stores kinds as strings in the token list instead, and this
   function is called on each string at the latest possible time. *)
let match_reference_kind warnings location s : _ Paths.Reference.tag =
  match s with
  | None -> TUnknown
  | Some s as wrapped ->
    let result =
      match match_ocamldoc_reference_kind warnings wrapped with
      | Some kind -> Some kind
      | None -> match_extra_odoc_reference_kind warnings wrapped
    in
    match result with
    | Some kind -> kind
    | None ->
      Parse_error.unknown_reference_qualifier s location
      |> Error.raise_exception

(* The string is scanned right-to-left, because we are interested in right-most
   hyphens. The tokens are also returned in right-to-left order, because the
   traversals that consume them prefer to look at the deepest identifier
   first. *)
let tokenize location s =
  let rec scan_identifier started_at open_parenthesis_count index tokens =
    match s.[index] with
    | exception Invalid_argument _ ->
      identifier_ended started_at index tokens
    | '-' | '.' when open_parenthesis_count = 0 ->
      identifier_ended started_at index tokens
    | ')' ->
      scan_identifier
        started_at (open_parenthesis_count + 1) (index - 1) tokens
    | '(' when open_parenthesis_count > 0 ->
      scan_identifier
        started_at (open_parenthesis_count - 1) (index - 1) tokens
    | _ ->
      scan_identifier
        started_at open_parenthesis_count (index - 1) tokens

  and identifier_ended started_at index tokens =
    let offset = index + 1 in
    let length = started_at - offset in
    let identifier = String.trim (String.sub s offset length) in
    let location = Location_.in_string s ~offset ~length location in

    if identifier = "" then begin
      Parse_error.should_not_be_empty ~what:"Identifier in reference" location
      |> Error.raise_exception
    end;

    match s.[index] with
    | exception Invalid_argument _ ->
      (None, identifier, location)::tokens
    | '.' ->
      scan_identifier index 0 (index - 1) ((None, identifier, location)::tokens)
    | '-' ->
      scan_kind identifier location index (index - 1) tokens
    | _ ->
      assert false

  and scan_kind identifier identifier_location started_at index tokens =
    match s.[index] with
    | exception Invalid_argument _ ->
      kind_ended identifier identifier_location started_at index tokens
    | '.' ->
      kind_ended identifier identifier_location started_at index tokens
    | _ ->
      scan_kind identifier identifier_location started_at (index - 1) tokens

  and kind_ended identifier identifier_location started_at index tokens =
    let offset = index + 1 in
    let length = started_at - offset in
    let kind = Some (String.sub s offset length) in
    let location = Location_.in_string s ~offset ~length location in
    let location = Location_.span [location; identifier_location] in

    match s.[index] with
    | exception Invalid_argument _ ->
      (kind, identifier, location)::tokens
    | '.' ->
      scan_identifier index 0 (index - 1) ((kind, identifier, location)::tokens)
    | _ ->
      assert false

  in

  scan_identifier (String.length s) 0 (String.length s - 1) []
  |> List.rev

let expected allowed location =
  let unqualified = "or an unqualified reference" in
  let allowed =
    match allowed with
    | [one] ->
      Printf.sprintf "'%s-' %s" one unqualified
    | _ ->
      String.concat
        ", " ((List.map (Printf.sprintf "'%s-'") allowed) @ [unqualified])
  in
  Parse_error.expected allowed location

let parse warnings location s =
  let open Paths.Reference in

  let rec signature (kind, identifier, location) tokens =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | TUnknown | TModule | TModuleType as kind -> Root (identifier, kind)
      | _ -> expected ["module"; "modtype"] location |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | TUnknown ->
        Dot (label_parent_of_parent (parent next_token tokens), identifier)
      | TModule -> Module (signature next_token tokens, identifier)
      | TModuleType -> ModuleType (signature next_token tokens, identifier)
      | _ -> expected ["module"; "modtype"] location |> Error.raise_exception
      end

  and parent (kind, identifier, location) tokens =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | TUnknown | TModule | TModuleType | TType | TClass
      | TClassType as kind -> Root (identifier, kind)
      | _ ->
        expected ["module"; "modtype"; "type"; "class"; "classtype"] location
        |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | TUnknown ->
        Dot (label_parent_of_parent (parent next_token tokens), identifier)
      | TModule -> Module (signature next_token tokens, identifier)
      | TModuleType -> ModuleType (signature next_token tokens, identifier)
      | TType -> Type (signature next_token tokens, identifier)
      | TClass -> Class (signature next_token tokens, identifier)
      | TClassType -> ClassType (signature next_token tokens, identifier)
      | _ ->
        expected ["module"; "modtype"; "type"; "class"; "classtype"] location
        |> Error.raise_exception
      end

  in

  let class_signature (kind, identifier, location) tokens =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | TUnknown | TClass | TClassType as kind -> Root (identifier, kind)
      | _ -> expected ["class"; "classtype"] location |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | TUnknown ->
        Dot (label_parent_of_parent (parent next_token tokens), identifier)
      | TClass -> Class (signature next_token tokens, identifier)
      | TClassType -> ClassType (signature next_token tokens, identifier)
      | _ -> expected ["class"; "classtype"] location |> Error.raise_exception
      end
  in

  let datatype (kind, identifier, location) tokens =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | TUnknown | TType as kind -> Root (identifier, kind)
      | _ -> expected ["type"] location |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | TUnknown ->
        Dot (label_parent_of_parent (parent next_token tokens), identifier)
      | TType -> Type (signature next_token tokens, identifier)
      | _ -> expected ["type"] location |> Error.raise_exception
      end
  in

  let rec label_parent (kind, identifier, location) tokens =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | TUnknown | TModule | TModuleType | TType | TClass | TClassType
      | TPage as kind -> Root (identifier, kind)
      | _ ->
        expected
          ["module"; "modtype"; "type"; "class"; "classtype"; "page"] location
        |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | TUnknown -> Dot (label_parent next_token tokens, identifier)
      | TModule -> Module (signature next_token tokens, identifier)
      | TModuleType -> ModuleType (signature next_token tokens, identifier)
      | TType -> Type (signature next_token tokens, identifier)
      | TClass -> Class (signature next_token tokens, identifier)
      | TClassType -> ClassType (signature next_token tokens, identifier)
      | _ ->
        expected ["module"; "modtype"; "type"; "class"; "classtype"] location
        |> Error.raise_exception
      end
  in

  let start_from_last_component (kind, identifier, location) tokens =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] -> Root (identifier, kind)
    | next_token::tokens ->
      match kind with
      | TUnknown -> Dot (label_parent next_token tokens, identifier)
      | TModule -> Module (signature next_token tokens, identifier)
      | TModuleType -> ModuleType (signature next_token tokens, identifier)
      | TType -> Type (signature next_token tokens, identifier)
      | TConstructor -> Constructor (datatype next_token tokens, identifier)
      | TField -> Field (parent next_token tokens, identifier)
      | TExtension -> Extension (signature next_token tokens, identifier)
      | TException -> Exception (signature next_token tokens, identifier)
      | TValue -> Value (signature next_token tokens, identifier)
      | TClass -> Class (signature next_token tokens, identifier)
      | TClassType -> ClassType (signature next_token tokens, identifier)
      | TMethod -> Method (class_signature next_token tokens, identifier)
      | TInstanceVariable ->
        InstanceVariable (class_signature next_token tokens, identifier)
      | TLabel -> Label (label_parent next_token tokens, identifier)
      | TPage ->
        let suggestion =
          Printf.sprintf "'page-%s' should be first." identifier in
        Parse_error.not_allowed
          ~what:"Page label"
          ~in_what:"the last component of a reference path"
          ~suggestion
          location
        |> Error.raise_exception
  in

  let s, location =
    match String.rindex s ':' with
    | index ->
      let s = String.sub s (index + 1) (String.length s - (index + 1)) in
      let location = Location_.nudge_start (index + 1) location in
      (s, location)
    | exception Not_found ->
      (s, location)
  in

  Error.catch begin fun () ->
    match tokenize location s with
    | [] -> assert false
    | last_token::tokens -> start_from_last_component last_token tokens
  end

let read_path_longident location s =
  let open Paths.Path in
  let rec loop : 'k. string -> int -> ([< kind > `Module ] as 'k) t option =
    fun s pos ->
      try
        let idx = String.rindex_from s pos '.' in
        let name = String.sub s (idx + 1) (pos - idx) in
        if String.length name = 0 then None
        else
          match loop s (idx - 1) with
          | None -> None
          | Some parent -> Some (Dot(parent, name))
      with Not_found ->
        let name = String.sub s 0 (pos + 1) in
        if String.length name = 0 then None
        else Some (Root name)
  in
  match loop s (String.length s - 1) with
  | Some r -> Result.Ok r
  | None -> Result.Error (Parse_error.expected "a valid path" location)

let read_mod_longident warnings location lid =
  let open Paths.Reference in
  let (>>=) = Rresult.(>>=) in

  parse warnings location lid >>= function
  | Root (_, (TUnknown | TModule))
  | Dot (_, _)
  | Module (_, _) as r -> Result.Ok r
  | _ -> Result.Error (Parse_error.expected "a reference to a module" location)
