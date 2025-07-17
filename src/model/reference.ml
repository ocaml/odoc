type path = [ `Root of string | `Dot of path * string ]

let expected_err :
    (Format.formatter -> 'a -> unit) -> 'a -> Location_.span -> Error.t =
 fun pp_a a -> Error.make "Expected %a." pp_a a

let expected_err_str : string -> Location_.span -> Error.t =
  expected_err Format.pp_print_string

let unknown_reference_qualifier : string -> Location_.span -> Error.t =
  Error.make "Unknown reference qualifier '%s'."

let deprecated_reference_kind : string -> string -> Location_.span -> Error.t =
  Error.make "'%s' is deprecated, use '%s' instead."

let reference_kinds_do_not_match : string -> string -> Location_.span -> Error.t
    =
  Error.make "Old-style reference kind ('%s:') does not match new ('%s-')."

let should_not_be_empty : what:string -> Location_.span -> Error.t =
 fun ~what ->
  Error.make "%s should not be empty." (Astring.String.Ascii.capitalize what)

let not_allowed :
    ?suggestion:string ->
    what:string ->
    in_what:string ->
    Location_.span ->
    Error.t =
 fun ?suggestion ~what ~in_what ->
  Error.make ?suggestion "%s is not allowed in %s."
    (Astring.String.Ascii.capitalize what)
    in_what

(** Format a list in a human readable way: [A, B, or C]. *)
let pp_hum_comma_separated pp_a ppf lst =
  let rec loop hd = function
    | [] -> Format.fprintf ppf "or %a" pp_a hd
    | hd' :: tl' ->
        Format.fprintf ppf "%a, " pp_a hd;
        loop hd' tl'
  in
  match lst with [] -> () | [ a ] -> pp_a ppf a | hd :: tl -> loop hd tl

let deprecated_reference_kind location kind replacement =
  deprecated_reference_kind kind replacement location |> Error.raise_warning

(* http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec359. *)
let match_ocamldoc_reference_kind (_location as loc) s :
    [> Paths.Reference.tag_any ] option =
  let d = deprecated_reference_kind in
  match s with
  | "module" -> Some `TModule
  | "modtype" ->
      d loc "modtype" "module-type";
      Some `TModuleType
  | "class" -> Some `TClass
  | "classtype" ->
      d loc "classtype" "class-type";
      Some `TClassType
  | "val" -> Some `TValue
  | "type" -> Some `TType
  | "exception" -> Some `TException
  | "attribute" -> None
  | "method" -> Some `TMethod
  | "section" -> Some `TLabel
  | "const" ->
      d loc "const" "constructor";
      Some `TConstructor
  | "recfield" ->
      d loc "recfield" "field";
      Some `TField
  | "childpage" -> Some `TChildPage
  | "childmodule" -> Some `TChildModule
  | _ -> None

let match_extra_odoc_reference_kind (_location as loc) s :
    [> Paths.Reference.tag_any ] option =
  let d = deprecated_reference_kind in
  match s with
  | "class-type" -> Some `TClassType
  | "constructor" -> Some `TConstructor
  | "exn" ->
      d loc "exn" "exception";
      Some `TException
  | "extension" -> Some `TExtension
  | "extension-decl" -> Some `TExtensionDecl
  | "field" -> Some `TField
  | "instance-variable" -> Some `TInstanceVariable
  | "label" ->
      d loc "label" "section";
      Some `TLabel
  | "module-type" -> Some `TModuleType
  | "page" -> Some `TPage
  | "asset" -> Some `TAsset
  | "value" ->
      d loc "value" "val";
      Some `TValue
  | _ -> None

type reference_kind = Paths.Reference.tag_any

(* Ideally, [tokenize] would call this on every reference kind annotation during
   tokenization, when generating the token list. However, that constrains the
   phantom tag type to be the same for all tokens in the list (because lists are
   homogeneous). So, the parser stores kinds as strings in the token list
   instead, and this function is called on each string at the latest possible
   time to prevent typing issues.

   A secondary reason to delay parsing, and store strings in the token list, is
   that we need the strings for user-friendly error reporting. *)
let match_reference_kind location s : reference_kind =
  match s with
  | `None -> `TUnknown
  | `Prefixed s | `Old_prefix s -> (
      let result =
        match match_ocamldoc_reference_kind location s with
        | Some _ as kind -> kind
        | None -> match_extra_odoc_reference_kind location s
      in
      match result with
      | Some kind -> kind
      | None -> unknown_reference_qualifier s location |> Error.raise_exception)

type token = {
  kind : [ `None | `Prefixed of string ];
  identifier : string;
  location : Location_.span;
}

type path_prefix = Path_prefix of string * Location_.span

(* The string is scanned right-to-left, because we are interested in right-most
   hyphens. The tokens are also returned in right-to-left order, because the
   traversals that consume them prefer to look at the deepest identifier
   first. *)
let tokenize location s : token list * path_prefix option =
  let rec scan_identifier started_at open_parenthesis_count index tokens =
    match s.[index] with
    | exception Invalid_argument _ ->
        let identifier, location = identifier_ended started_at index in
        ({ kind = `None; identifier; location } :: tokens, None)
    | '-' when open_parenthesis_count = 0 ->
        let identifier, location = identifier_ended started_at index in
        scan_kind identifier location index (index - 1) tokens
    | '.' when open_parenthesis_count = 0 ->
        let identifier, location = identifier_ended started_at index in
        scan_identifier index 0 (index - 1)
          ({ kind = `None; identifier; location } :: tokens)
    | '/' when open_parenthesis_count = 0 ->
        let identifier, location = identifier_ended started_at index in
        scan_path index ({ kind = `None; identifier; location } :: tokens)
    | ')' ->
        scan_identifier started_at
          (open_parenthesis_count + 1)
          (index - 1) tokens
    | '(' when open_parenthesis_count > 0 ->
        scan_identifier started_at
          (open_parenthesis_count - 1)
          (index - 1) tokens
    | '"' -> (
        try
          scan_identifier started_at 0
            (String.rindex_from s (index - 1) '"' - 1)
            tokens
        with _ ->
          Error.raise_exception (Error.make "Unmatched quotation!" location))
    | _ -> scan_identifier started_at open_parenthesis_count (index - 1) tokens
  and identifier_ended started_at index =
    let offset = index + 1 in
    let length = started_at - offset in
    let identifier = String.sub s offset length in
    let identifier =
      Astring.String.cuts ~sep:"\"" identifier
      |> List.mapi (fun i s ->
             if i mod 2 = 0 then
               Astring.String.cuts s ~sep:" " |> String.concat ""
             else s)
      |> String.concat ""
    in
    let location = Location_.in_string s ~offset ~length location in

    if identifier = "" then
      should_not_be_empty ~what:"Identifier in reference" location
      |> Error.raise_exception;

    (identifier, location)
  and scan_kind identifier identifier_location started_at index tokens =
    match s.[index] with
    | exception Invalid_argument _ ->
        let kind, location = kind_ended identifier_location started_at index in
        ({ kind; identifier; location } :: tokens, None)
    | '.' ->
        let kind, location = kind_ended identifier_location started_at index in
        scan_identifier index 0 (index - 1)
          ({ kind; identifier; location } :: tokens)
    | '/' ->
        let kind, location = kind_ended identifier_location started_at index in
        scan_path index ({ kind; identifier; location } :: tokens)
    | _ ->
        scan_kind identifier identifier_location started_at (index - 1) tokens
  and kind_ended identifier_location started_at index =
    let offset = index + 1 in
    let length = started_at - offset in
    let kind = `Prefixed (String.sub s offset length) in
    let location = Location_.in_string s ~offset ~length location in
    let location = Location_.span [ location; identifier_location ] in
    (kind, location)
  and scan_path started_at tokens =
    let location =
      Location_.in_string s ~offset:0 ~length:(started_at + 1) location
    in
    (tokens, Some (Path_prefix (String.sub s 0 (started_at + 1), location)))
  in

  scan_identifier (String.length s) 0 (String.length s - 1) []
  |> fun (toks, p) -> (List.rev toks, p)

let expected ?(expect_paths = false) allowed location =
  let unqualified = [ "an unqualified reference" ] in
  let unqualified =
    if expect_paths then "a path" :: unqualified else unqualified
  in
  let allowed = List.map (Printf.sprintf "'%s-'") allowed @ unqualified in
  expected_err (pp_hum_comma_separated Format.pp_print_string) allowed location

let parse_path whole_path_location p =
  let segs = Astring.String.cuts ~sep:"/" p in
  let check segs start =
    let _finish =
      List.fold_left
        (fun offset seg ->
          match seg with
          | "" ->
              let location =
                Location_.in_string p ~offset ~length:0 whole_path_location
              in
              should_not_be_empty ~what:"Identifier in path reference" location
              |> Error.raise_exception
          | seg -> offset + String.length seg + 1)
        start segs
    in
    ()
  in
  match segs with
  | "." :: segs ->
      check segs 2;
      (`TRelativePath, segs)
  | "" :: "" :: segs ->
      check segs 2;
      (`TCurrentPackage, segs)
  | "" :: segs ->
      check segs 1;
      (`TAbsolutePath, segs)
  | segs ->
      check segs 0;
      (`TRelativePath, segs)

let parse_path_prefix (Path_prefix (p, path_location)) identifier
    prefix_location =
  parse_path (Location_.span [ path_location; prefix_location ]) (p ^ identifier)

(* Parse references that do not contain a [/]. Raises errors and warnings. *)
let parse whole_reference_location s :
    Paths.Reference.t Error.with_errors_and_warnings =
  let open Paths.Reference in
  let open Names in
  let parse_from_last_component { kind; identifier; location } old_kind tokens
      path_prefix =
    let rec signature { kind; identifier; location } tokens : Signature.t =
      let kind = match_reference_kind location kind in
      match tokens with
      | [] -> (
          match path_prefix with
          | None -> (
              match kind with
              | (`TUnknown | `TModule | `TModuleType) as kind ->
                  `Root (identifier, kind)
              | _ ->
                  expected ~expect_paths:true
                    [ "module"; "module-type" ]
                    location
                  |> Error.raise_exception)
          | Some p -> (
              match kind with
              | `TUnknown | `TModule ->
                  `Module_path (parse_path_prefix p identifier location)
              | _ ->
                  expected ~expect_paths:true [ "module" ] location
                  |> Error.raise_exception))
      | next_token :: tokens -> (
          match kind with
          | `TUnknown ->
              `Dot ((parent next_token tokens :> LabelParent.t), identifier)
          | `TModule ->
              `Module
                (signature next_token tokens, ModuleName.make_std identifier)
          | `TModuleType ->
              `ModuleType
                (signature next_token tokens, ModuleTypeName.make_std identifier)
          | _ ->
              expected ~expect_paths:true [ "module"; "module-type" ] location
              |> Error.raise_exception)
    and parent { kind; identifier; location } tokens : FragmentTypeParent.t =
      let kind = match_reference_kind location kind in
      match tokens with
      | [] -> (
          match path_prefix with
          | None -> (
              match kind with
              | (`TUnknown | `TModule | `TModuleType | `TType) as kind ->
                  `Root (identifier, kind)
              | _ ->
                  expected [ "module"; "module-type"; "type" ] location
                  |> Error.raise_exception)
          | Some p -> (
              match kind with
              | `TUnknown | `TModule ->
                  `Module_path (parse_path_prefix p identifier location)
              | _ ->
                  expected ~expect_paths:true [ "module" ] location
                  |> Error.raise_exception))
      | next_token :: tokens -> (
          match kind with
          | `TUnknown ->
              `Dot ((parent next_token tokens :> LabelParent.t), identifier)
          | `TModule ->
              `Module
                (signature next_token tokens, ModuleName.make_std identifier)
          | `TModuleType ->
              `ModuleType
                (signature next_token tokens, ModuleTypeName.make_std identifier)
          | `TType ->
              `Type (signature next_token tokens, TypeName.make_std identifier)
          | _ ->
              expected [ "module"; "module-type"; "type" ] location
              |> Error.raise_exception)
    in

    let class_signature { kind; identifier; location } tokens : ClassSignature.t
        =
      let kind = match_reference_kind location kind in
      match tokens with
      | [] -> (
          match kind with
          | (`TUnknown | `TClass | `TClassType) as kind ->
              `Root (identifier, kind)
          | _ ->
              expected [ "class"; "class-type" ] location
              |> Error.raise_exception)
      | next_token :: tokens -> (
          match kind with
          | `TUnknown ->
              `Dot ((parent next_token tokens :> LabelParent.t), identifier)
          | `TClass ->
              `Class (signature next_token tokens, TypeName.make_std identifier)
          | `TClassType ->
              `ClassType
                (signature next_token tokens, TypeName.make_std identifier)
          | _ ->
              expected [ "class"; "class-type" ] location
              |> Error.raise_exception)
    in

    let label_parent_path kind path_prefix identifier location =
      match kind with
      | `TUnknown ->
          `Any_path (parse_path_prefix path_prefix identifier location)
      | `TModule ->
          `Module_path (parse_path_prefix path_prefix identifier location)
      | `TPage -> `Page_path (parse_path_prefix path_prefix identifier location)
      | _ ->
          expected ~expect_paths:true [ "module"; "page" ] location
          |> Error.raise_exception
    in

    let any_path kind path_prefix identifier location =
      match kind with
      | `TUnknown ->
          `Any_path (parse_path_prefix path_prefix identifier location)
      | `TModule ->
          `Module_path (parse_path_prefix path_prefix identifier location)
      | `TPage -> `Page_path (parse_path_prefix path_prefix identifier location)
      | `TAsset ->
          `Asset_path (parse_path_prefix path_prefix identifier location)
      | _ ->
          expected ~expect_paths:true [ "module"; "page" ] location
          |> Error.raise_exception
    in

    let rec label_parent { kind; identifier; location } tokens : LabelParent.t =
      let kind = match_reference_kind location kind in
      match tokens with
      | [] -> (
          match path_prefix with
          | None -> (
              match kind with
              | ( `TUnknown | `TModule | `TModuleType | `TType | `TClass
                | `TClassType | `TPage ) as kind ->
                  `Root (identifier, kind)
              | _ ->
                  expected ~expect_paths:true
                    [
                      "module";
                      "module-type";
                      "type";
                      "class";
                      "class-type";
                      "page";
                    ]
                    location
                  |> Error.raise_exception)
          | Some p -> label_parent_path kind p identifier location)
      | next_token :: tokens -> (
          match kind with
          | `TUnknown -> `Dot (label_parent next_token tokens, identifier)
          | `TModule ->
              `Module
                (signature next_token tokens, ModuleName.make_std identifier)
          | `TModuleType ->
              `ModuleType
                (signature next_token tokens, ModuleTypeName.make_std identifier)
          | `TType ->
              `Type (signature next_token tokens, TypeName.make_std identifier)
          | `TClass ->
              `Class (signature next_token tokens, TypeName.make_std identifier)
          | `TClassType ->
              `ClassType
                (signature next_token tokens, TypeName.make_std identifier)
          | _ ->
              expected ~expect_paths:true
                [ "module"; "module-type"; "type"; "class"; "class-type" ]
                location
              |> Error.raise_exception)
    in

    let start_from_last_component { kind; identifier; location } old_kind tokens
        =
      let new_kind = match_reference_kind location kind in
      let kind =
        match old_kind with
        | None -> new_kind
        | Some (old_kind_string, old_kind_location) -> (
            let old_kind =
              match_reference_kind old_kind_location
                (`Old_prefix old_kind_string)
            in
            match new_kind with
            | `TUnknown -> old_kind
            | _ ->
                (if old_kind <> new_kind then
                   let new_kind_string =
                     match kind with `None -> "" | `Prefixed s -> s
                   in
                   reference_kinds_do_not_match old_kind_string new_kind_string
                     whole_reference_location
                   |> Error.raise_warning);
                new_kind)
      in

      match tokens with
      | [] -> (
          match path_prefix with
          | None -> `Root (identifier, kind)
          | Some p -> any_path kind p identifier location)
      | next_token :: tokens -> (
          match kind with
          | `TUnknown -> `Dot (label_parent next_token tokens, identifier)
          | `TModule ->
              `Module
                (signature next_token tokens, ModuleName.make_std identifier)
          | `TModuleType ->
              `ModuleType
                (signature next_token tokens, ModuleTypeName.make_std identifier)
          | `TType ->
              `Type (signature next_token tokens, TypeName.make_std identifier)
          | `TConstructor ->
              `Constructor
                (parent next_token tokens, ConstructorName.make_std identifier)
          | `TField ->
              `Field (parent next_token tokens, FieldName.make_std identifier)
          | `TUnboxedField ->
              `UnboxedField (parent next_token tokens, UnboxedFieldName.make_std identifier)
          | `TExtension ->
              `Extension
                (signature next_token tokens, ExtensionName.make_std identifier)
          | `TExtensionDecl ->
              `ExtensionDecl
                (signature next_token tokens, ExtensionName.make_std identifier)
          | `TException ->
              `Exception
                (signature next_token tokens, ExceptionName.make_std identifier)
          | `TValue ->
              `Value (signature next_token tokens, ValueName.make_std identifier)
          | `TClass ->
              `Class (signature next_token tokens, TypeName.make_std identifier)
          | `TClassType ->
              `ClassType
                (signature next_token tokens, TypeName.make_std identifier)
          | `TMethod ->
              `Method
                ( class_signature next_token tokens,
                  MethodName.make_std identifier )
          | `TInstanceVariable ->
              `InstanceVariable
                ( class_signature next_token tokens,
                  InstanceVariableName.make_std identifier )
          | `TLabel ->
              `Label
                (label_parent next_token tokens, LabelName.make_std identifier)
          | `TChildPage | `TChildModule ->
              let suggestion =
                Printf.sprintf "'child-%s' should be first." identifier
              in
              not_allowed ~what:"Child label"
                ~in_what:"the last component of a reference path" ~suggestion
                location
              |> Error.raise_exception
          | `TPage ->
              let suggestion =
                Printf.sprintf "Reference pages as '<parent_path>/%s'."
                  identifier
              in
              not_allowed ~what:"Page label"
                ~in_what:"on the right side of a dot" ~suggestion location
              |> Error.raise_exception
          | `TAsset ->
              let suggestion =
                Printf.sprintf "Reference assets as '<parent_path>/%s'."
                  identifier
              in
              not_allowed ~what:"Asset label"
                ~in_what:"on the right side of a dot" ~suggestion location
              |> Error.raise_exception)
    in
    start_from_last_component { kind; identifier; location } old_kind tokens
  in
  Error.catch_errors_and_warnings (fun () ->
      let old_kind, s, location =
        let rec find_old_reference_kind_separator index =
          if index < 0 then raise Not_found
          else
            match s.[index] with
            | ':' -> index
            | ')' -> (
                match String.rindex_from s index '(' with
                | index -> find_old_reference_kind_separator (index - 1)
                | exception (Not_found as exn) -> raise exn)
            | _ -> find_old_reference_kind_separator (index - 1)
        in
        match find_old_reference_kind_separator (String.length s - 1) with
        | index ->
            let old_kind = String.trim (String.sub s 0 index) in
            let old_kind_location =
              Location_.set_end_as_offset_from_start index
                whole_reference_location
            in
            let s = String.sub s (index + 1) (String.length s - (index + 1)) in
            let location =
              Location_.nudge_start (index + 1) whole_reference_location
            in
            (Some (old_kind, old_kind_location), s, location)
        | exception Not_found -> (None, s, whole_reference_location)
      in
      match tokenize location s with
      | last_token :: tokens, path_prefix ->
          parse_from_last_component last_token old_kind tokens path_prefix
      | [], _ ->
          should_not_be_empty ~what:"Reference target" whole_reference_location
          |> Error.raise_exception)

(* Parse references that do not contain a [/]. Raises errors and warnings. *)
let parse_asset whole_reference_location s :
    Paths.Reference.Asset.t Error.with_errors_and_warnings =
  let path = parse_path whole_reference_location s in
  Error.catch_errors_and_warnings (fun () -> `Asset_path path)

let read_path_longident location s =
  let rec loop : string -> int -> path option =
   fun s pos ->
    try
      let idx = String.rindex_from s pos '.' in
      let name = String.sub s (idx + 1) (pos - idx) in
      if String.length name = 0 then None
      else
        match loop s (idx - 1) with
        | None -> None
        | Some parent -> Some (`Dot (parent, name))
    with Not_found ->
      let name = String.sub s 0 (pos + 1) in
      if String.length name = 0 then None else Some (`Root name)
  in
  Error.catch_warnings (fun () ->
      match loop s (String.length s - 1) with
      | Some r -> Ok (r :> path)
      | None -> Error (expected_err_str "a valid path" location))

let read_mod_longident location lid =
  Error.catch_warnings (fun () ->
      match Error.raise_warnings (parse location lid) with
      | Error _ as e -> e
      | Ok p -> (
          match p with
          | (`Root (_, (`TUnknown | `TModule)) | `Dot (_, _) | `Module (_, _))
            as r ->
              Ok r
          | _ -> Error (expected_err_str "a reference to a module" location)))
