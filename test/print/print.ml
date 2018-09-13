open Result

type sexp = Sexplib.Sexp.t =
  | Atom of string
  | List of sexp list



module Root_to_sexp =
struct
  module Root = Model.Root

  let odoc_file : Root.Odoc_file.t -> sexp = function
    | Page p ->
      List [Atom "page"; Atom p]
    | Compilation_unit {name; hidden} ->
      let hidden = if hidden then [Atom "hidden"] else [] in
      List ((Atom "compilation_unit")::(Atom name)::hidden)

  let root : Root.t -> sexp = fun {package; file; digest} ->
    List [Atom package; odoc_file file; Atom (Digest.to_hex digest)]
end



module Identifier_to_sexp =
struct
  module Identifier = Model.Paths.Identifier

  let identifier : _ Identifier.t -> sexp =
    let rec traverse : type k. sexp list -> k Identifier.t -> sexp =
        fun acc -> function
      | Identifier.Root (root, s) ->
        List ((List [Atom "root"; Root_to_sexp.root root; Atom s])::acc)
      | Identifier.Page (root, s) ->
        List ((List [Atom "root"; Root_to_sexp.root root; Atom s])::acc)
      | Identifier.Module (parent, s) ->
        traverse ((List [Atom "module"; Atom s])::acc) parent
      | Identifier.Argument (parent, i, s) ->
        traverse
          ((List [Atom "argument"; Atom (string_of_int i); Atom s])::acc) parent
      | Identifier.ModuleType (parent, s) ->
        traverse ((List [Atom "module_type"; Atom s])::acc) parent
      | Identifier.Type (parent, s) ->
        traverse ((List [Atom "type"; Atom s])::acc) parent
      | Identifier.CoreType s ->
        List ((List [Atom "core_type"; Atom s])::acc)
      | Identifier.Constructor (parent, s) ->
        traverse ((List [Atom "constructor"; Atom s])::acc) parent
      | Identifier.Field (parent, s) ->
        traverse ((List [Atom "field"; Atom s])::acc) parent
      | Identifier.Extension (parent, s) ->
        traverse ((List [Atom "extension"; Atom s])::acc) parent
      | Identifier.Exception (parent, s) ->
        traverse ((List [Atom "exception"; Atom s])::acc) parent
      | Identifier.CoreException s ->
        List ((List [Atom "core_exception"; Atom s])::acc)
      | Identifier.Value (parent, s) ->
        traverse ((List [Atom "value"; Atom s])::acc) parent
      | Identifier.Class (parent, s) ->
        traverse ((List [Atom "class"; Atom s])::acc) parent
      | Identifier.ClassType (parent, s) ->
        traverse ((List [Atom "class_type"; Atom s])::acc) parent
      | Identifier.Method (parent, s) ->
        traverse ((List [Atom "method"; Atom s])::acc) parent
      | Identifier.InstanceVariable (parent, s) ->
        traverse ((List [Atom "instance_variable"; Atom s])::acc) parent
      | Identifier.Label (parent, s) ->
        traverse ((List [Atom "label"; Atom s])::acc) parent
    in
    fun path ->
      traverse [] path
end



module Path_to_sexp =
struct
  module Path = Model.Paths.Path
  module Resolved = Model.Paths.Path.Resolved

  let rec path : type k. k Path.t -> sexp = function
    | Path.Resolved parent ->
      List [Atom "resolved"; resolved parent]
    | Path.Root s ->
      List [Atom "root"; Atom s]
    | Path.Forward s ->
      List [Atom "forward"; Atom s]
    | Path.Dot (parent, s) ->
      List [Atom "dot"; Atom s; path parent]
    | Path.Apply (m, m') ->
      List [Atom "apply"; path m; path m']

  and resolved : type k. k Resolved.t -> sexp = function
    | Resolved.Identifier i ->
      List [Atom "identifier"; Identifier_to_sexp.identifier i]
    | Resolved.Subst (mt, m) ->
      List [Atom "subst"; resolved mt; resolved m]
    | Resolved.SubstAlias (m, m') ->
      List [Atom "subst_alias"; resolved m; resolved m']
    | Resolved.Hidden m ->
      List [Atom "hidden"; resolved m]
    | Resolved.Module (m, s) ->
      List [Atom "module"; Atom s; resolved m]
    | Resolved.Canonical (m, p) ->
      List [Atom "canonical"; resolved m; path p]
    | Resolved.Apply (m, p) ->
      List [Atom "apply"; resolved m; path p]
    | Resolved.ModuleType (m, s) ->
      List [Atom "module_type"; Atom s; resolved m]
    | Resolved.Type (m, s) ->
      List [Atom "type"; Atom s; resolved m]
    | Resolved.Class (m, s) ->
      List [Atom "class"; Atom s; resolved m]
    | Resolved.ClassType (m, s) ->
      List [Atom "class_type"; Atom s; resolved m]
end



module Reference_to_sexp =
struct
  module Reference = Model.Paths.Reference
  module Resolved = Model.Paths.Reference.Resolved

  let tag : type k. k Reference.tag -> sexp = function
    | Reference.TUnknown -> Atom "unknown"
    | Reference.TModule -> Atom "module"
    | Reference.TModuleType -> Atom "module_type"
    | Reference.TType -> Atom "type"
    | Reference.TConstructor -> Atom "constructor"
    | Reference.TField -> Atom "field"
    | Reference.TExtension -> Atom "extension"
    | Reference.TException -> Atom "exception"
    | Reference.TValue -> Atom "value"
    | Reference.TClass -> Atom "class"
    | Reference.TClassType -> Atom "class_type"
    | Reference.TMethod -> Atom "method"
    | Reference.TInstanceVariable -> Atom "instance_variable"
    | Reference.TLabel -> Atom "label"
    | Reference.TPage -> Atom "page"

  let rec reference : type k. k Reference.t -> sexp = function
    | Reference.Resolved parent ->
      List [Atom "resolved"; resolved parent]
    | Reference.Root (s, k) ->
      List [Atom "root"; Atom s; tag k]
    | Reference.Dot (parent, s) ->
      List [Atom "dot"; Atom s; reference parent]
    | Reference.Module (parent, s) ->
      List [Atom "module"; Atom s; reference parent]
    | Reference.ModuleType (parent, s) ->
      List [Atom "module_type"; Atom s; reference parent]
    | Reference.Type (parent, s) ->
      List [Atom "type"; Atom s; reference parent]
    | Reference.Constructor (parent, s) ->
      List [Atom "constructor"; Atom s; reference parent]
    | Reference.Field (parent, s) ->
      List [Atom "field"; Atom s; reference parent]
    | Reference.Extension (parent, s) ->
      List [Atom "extension"; Atom s; reference parent]
    | Reference.Exception (parent, s) ->
      List [Atom "exception"; Atom s; reference parent]
    | Reference.Value (parent, s) ->
      List [Atom "value"; Atom s; reference parent]
    | Reference.Class (parent, s) ->
      List [Atom "class"; Atom s; reference parent]
    | Reference.ClassType (parent, s) ->
      List [Atom "class_type"; Atom s; reference parent]
    | Reference.Method (parent, s) ->
      List [Atom "method"; Atom s; reference parent]
    | Reference.InstanceVariable (parent, s) ->
      List [Atom "instance_variable"; Atom s; reference parent]
    | Reference.Label (parent, s) ->
      List [Atom "label"; Atom s; reference parent]

  and resolved : type k. k Resolved.t -> sexp = function
    | Resolved.Identifier parent ->
      List [Atom "identifier"; Identifier_to_sexp.identifier parent]
    | Resolved.SubstAlias (m, m') ->
      List [Atom "subst_alias"; Path_to_sexp.resolved m; resolved m']
    | Resolved.Module (parent, s) ->
      List [Atom "module"; Atom s; resolved parent]
    | Resolved.Canonical (m, m') ->
      List [Atom "canonical"; resolved m; reference m']
    | Resolved.ModuleType (parent, s) ->
      List [Atom "module_type"; Atom s; resolved parent]
    | Resolved.Type (parent, s) ->
      List [Atom "type"; Atom s; resolved parent]
    | Resolved.Constructor (parent, s) ->
      List [Atom "constructor"; Atom s; resolved parent]
    | Resolved.Field (parent, s) ->
      List [Atom "field"; Atom s; resolved parent]
    | Resolved.Extension (parent, s) ->
      List [Atom "extension"; Atom s; resolved parent]
    | Resolved.Exception (parent, s) ->
      List [Atom "exception"; Atom s; resolved parent]
    | Resolved.Value (parent, s) ->
      List [Atom "value"; Atom s; resolved parent]
    | Resolved.Class (parent, s) ->
      List [Atom "class"; Atom s; resolved parent]
    | Resolved.ClassType (parent, s) ->
      List [Atom "class_type"; Atom s; resolved parent]
    | Resolved.Method (parent, s) ->
      List [Atom "method"; Atom s; resolved parent]
    | Resolved.InstanceVariable (parent, s) ->
      List [Atom "instance_variable"; Atom s; resolved parent]
    | Resolved.Label (parent, s) ->
      List [Atom "label"; Atom s; resolved parent]
end



module Location_to_sexp =
struct
  module Location_ = Model.Location_

  let point : Location_.point -> sexp = fun {line; column} ->
    List [Atom (string_of_int line); Atom (string_of_int column)]

  let span : Location_.span -> sexp = fun {file; start; end_} ->
    List [Atom file; point start; point end_]

  let at : ('a -> sexp) -> 'a Location_.with_location -> sexp =
      fun f {location; value} ->
    List [span location; f value]
end



module Comment_to_sexp =
struct
  module Comment = Model.Comment
  let at = Location_to_sexp.at

  let style : Comment.style -> sexp = function
    | `Bold -> Atom "bold"
    | `Italic -> Atom "italic"
    | `Emphasis -> Atom "emphasis"
    | `Superscript -> Atom "superscript"
    | `Subscript -> Atom "subscript"

  let leaf_inline_element : Comment.leaf_inline_element -> sexp =
    function
    | `Space -> Atom "space"
    | `Word w -> List [Atom "word"; Atom w]
    | `Code_span c -> List [Atom "code_span"; Atom c]
    | `Raw_markup (`Html, s) -> List [Atom "raw_markup"; Atom "html"; Atom s]

  let rec non_link_inline_element : Comment.non_link_inline_element -> sexp =
    function
    | #Comment.leaf_inline_element as e ->
      leaf_inline_element e
    | `Styled (s, es) ->
      List [style s; List (List.map (at non_link_inline_element) es)]

  let rec inline_element : Comment.inline_element -> sexp = function
    | #Comment.leaf_inline_element as e ->
      leaf_inline_element e
    | `Styled (s, es) ->
      List [style s; List (List.map (at inline_element) es)]
    | `Reference (r, es) ->
      List [
        Atom "reference";
        Reference_to_sexp.reference r;
        List (List.map (at non_link_inline_element) es)
      ]
    | `Link (u, es) ->
      List [
        Atom "link";
        Atom u;
        List (List.map (at non_link_inline_element) es)
      ]

  let rec nestable_block_element
      : Comment.nestable_block_element -> sexp =
    function
    | `Paragraph es ->
      List [Atom "paragraph"; List (List.map (at inline_element) es)]
    | `Code_block c -> List [Atom "code_block"; Atom c]
    | `Verbatim t -> List [Atom "verbatim"; Atom t]
    | `Modules ps ->
      List [Atom "modules"; List (List.map Reference_to_sexp.reference ps)]
    | `List (kind, items) ->
      let kind =
        match kind with
        | `Unordered -> "unordered"
        | `Ordered -> "ordered"
      in
      let items =
        items
        |> List.map (fun item ->
          List (List.map (at nestable_block_element) item))
        |> fun items -> List items
      in
      List [Atom kind; items]

  let tag : Comment.tag -> sexp = function
    | `Author s ->
      List [Atom "@author"; Atom s]
    | `Deprecated es ->
      List ((Atom "@deprecated")::(List.map (at nestable_block_element) es))
    | `Param (s, es) ->
      List ([Atom "@param"; Atom s] @ (List.map (at nestable_block_element) es))
    | `Raise (s, es) ->
      List ([Atom "@raise"; Atom s] @ (List.map (at nestable_block_element) es))
    | `Return es ->
      List ((Atom "@return")::(List.map (at nestable_block_element) es))
    | `See (kind, s, es) ->
      let kind =
        match kind with
        | `Url -> "url"
        | `File -> "file"
        | `Document -> "document"
      in
      List
        ([Atom "@see"; Atom kind; Atom s] @
          (List.map (at nestable_block_element) es))
    | `Since s -> List [Atom "@since"; Atom s]
    | `Before (s, es) ->
      List ([Atom "@before"; Atom s] @
        (List.map (at nestable_block_element) es))
    | `Version s -> List [Atom "@version"; Atom s]
    | `Canonical (p, r) ->
      List
        [Atom "@canonical"; Path_to_sexp.path p; Reference_to_sexp.reference r]
    | `Inline ->
      Atom "@inline"
    | `Open ->
      Atom "@open"
    | `Closed ->
      Atom "@closed"

  let block_element : Comment.block_element -> sexp = function
    | #Comment.nestable_block_element as e -> nestable_block_element e
    | `Heading (level, label, es) ->
      let label = List [Atom "label"; Identifier_to_sexp.identifier label] in
      let level =
        match level with
        | `Title -> "1"
        | `Section -> "2"
        | `Subsection -> "3"
        | `Subsubsection -> "4"
      in
      List [Atom level; label; List (List.map (at non_link_inline_element) es)]
    | `Tag t -> tag t

  let comment : Comment.docs -> sexp = fun comment ->
    List (List.map (at block_element) comment)
end



module Error_to_sexp =
struct
  let error : Model.Error.t -> sexp = fun error ->
    Atom (Model.Error.to_string error)
end



let parser_output formatter {Model.Error.result; warnings} =
  let result =
    match result with
    | Ok comment -> List [Atom "ok"; Comment_to_sexp.comment comment]
    | Error error -> List [Atom "error"; Error_to_sexp.error error]
  in
  let warnings = List (List.map Error_to_sexp.error warnings) in
  let output =
    List [
      List [Atom "output"; result];
      List [Atom "warnings"; warnings];
    ]
  in
  Sexplib.Sexp.pp_hum formatter output;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
