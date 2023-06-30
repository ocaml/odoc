module Location = Location_
module Ast = Odoc_parser.Ast

type internal_tags_removed =
  [ `Tag of Ast.ocamldoc_tag
  | `Heading of Ast.heading
  | Ast.nestable_block_element ]
(** {!Ast.block_element} without internal tags. *)

type _ handle_internal_tags =
  | Expect_status
      : [ `Default | `Inline | `Open | `Closed ] handle_internal_tags
  | Expect_canonical
      : [ `Dot of Paths.Path.Module.t * string ] option handle_internal_tags
  | Expect_none : unit handle_internal_tags

let describe_internal_tag = function
  | `Canonical _ -> "@canonical"
  | `Inline -> "@inline"
  | `Open -> "@open"
  | `Closed -> "@closed"
  | `Hidden -> "@hidden"

let warn_unexpected_tag { Location.value; location } =
  Error.raise_warning
  @@ Error.make "Unexpected tag '%s' at this location."
       (describe_internal_tag value)
       location

let warn_root_canonical location =
  Error.raise_warning
  @@ Error.make "Canonical paths must contain a dot, eg. X.Y." location

let rec find_tag f = function
  | [] -> None
  | hd :: tl -> (
      match f hd.Location.value with
      | Some x -> Some (x, hd.location)
      | None ->
          warn_unexpected_tag hd;
          find_tag f tl)

let handle_internal_tags (type a) tags : a handle_internal_tags -> a = function
  | Expect_status -> (
      match
        find_tag
          (function (`Inline | `Open | `Closed) as t -> Some t | _ -> None)
          tags
      with
      | Some (status, _) -> status
      | None -> `Default)
  | Expect_canonical -> (
      match find_tag (function `Canonical p -> Some p | _ -> None) tags with
      | Some (`Root _, location) ->
          warn_root_canonical location;
          None
      | Some ((`Dot _ as p), _) -> Some p
      | None -> None)
  | Expect_none ->
      (* Will raise warnings. *)
      ignore (find_tag (fun _ -> None) tags);
      ()

(* Errors *)
let invalid_raw_markup_target : string -> Location.span -> Error.t =
  Error.make ~suggestion:"try '{%html:...%}'."
    "'{%%%s:': bad raw markup target."

let default_raw_markup_target_not_supported : Location.span -> Error.t =
  Error.make ~suggestion:"try '{%html:...%}'."
    "'{%%...%%}' (raw markup) needs a target language."

let headings_not_allowed : Location.span -> Error.t =
  Error.make "Headings not allowed in this comment."

let titles_not_allowed : Location.span -> Error.t =
  Error.make "Title-level headings {0 ...} are only allowed in pages."

let bad_heading_level : int -> Location.span -> Error.t =
  Error.make "'%d': bad heading level (0-5 allowed)."

let heading_level_should_be_lower_than_top_level :
    int -> int -> Location.span -> Error.t =
 fun this_heading_level top_heading_level ->
  Error.make "%s: heading level should be lower than top heading level '%d'."
    (Printf.sprintf "'{%i'" this_heading_level)
    top_heading_level

let page_heading_required : string -> Error.t =
  Error.filename_only "Pages (.mld files) should start with a heading."

let tags_not_allowed : Location.span -> Error.t =
  Error.make "Tags are not allowed in pages."

let not_allowed :
    ?suggestion:string ->
    what:string ->
    in_what:string ->
    Location.span ->
    Error.t =
 fun ?suggestion ~what ~in_what ->
  Error.make ?suggestion "%s is not allowed in %s."
    (Astring.String.Ascii.capitalize what)
    in_what

let describe_element = function
  | `Reference (`Simple, _, _) -> "'{!...}' (cross-reference)"
  | `Reference (`With_text, _, _) -> "'{{!...} ...}' (cross-reference)"
  | `Link _ -> "'{{:...} ...}' (external link)"
  | `Heading (level, _, _) ->
      Printf.sprintf "'{%i ...}' (section heading)" level

(* End of errors *)

type 'a with_location = 'a Location.with_location

type ast_leaf_inline_element =
  [ `Space of string
  | `Word of string
  | `Code_span of string
  | `Math_span of string
  | `Raw_markup of string option * string ]

type sections_allowed = [ `All | `No_titles | `None ]

type alerts =
  [ `Tag of [ `Alert of string * string option ] ] Location_.with_location list

type status = {
  sections_allowed : sections_allowed;
  tags_allowed : bool;
  parent_of_sections : Paths.Identifier.LabelParent.t;
}

let leaf_inline_element :
    ast_leaf_inline_element with_location ->
    Comment.leaf_inline_element with_location =
 fun element ->
  match element with
  | { value = `Word _ | `Code_span _ | `Math_span _; _ } as element -> element
  | { value = `Space _; _ } -> Location.same element `Space
  | { value = `Raw_markup (target, s); location } -> (
      match target with
      | Some invalid_target
        when String.trim invalid_target = ""
             || String.contains invalid_target '%'
             || String.contains invalid_target '}' ->
          Error.raise_warning
            (invalid_raw_markup_target invalid_target location);

          Location.same element (`Code_span s)
      | None ->
          Error.raise_warning (default_raw_markup_target_not_supported location);
          Location.same element (`Code_span s)
      | Some target -> Location.same element (`Raw_markup (target, s)))

type surrounding =
  [ `Link of
    string * Odoc_parser.Ast.inline_element Location_.with_location list
  | `Reference of
    [ `Simple | `With_text ]
    * string Location_.with_location
    * Odoc_parser.Ast.inline_element Location_.with_location list ]

let rec non_link_inline_element :
    status ->
    surrounding:surrounding ->
    Odoc_parser.Ast.inline_element with_location ->
    Comment.non_link_inline_element with_location =
 fun status ~surrounding element ->
  match element with
  | { value = #ast_leaf_inline_element; _ } as element ->
      (leaf_inline_element element
        :> Comment.non_link_inline_element with_location)
  | { value = `Styled (style, content); _ } ->
      `Styled (style, non_link_inline_elements status ~surrounding content)
      |> Location.same element
  | ( { value = `Reference (_, _, content); _ }
    | { value = `Link (_, content); _ } ) as element ->
      not_allowed
        ~what:(describe_element element.value)
        ~in_what:(describe_element surrounding)
        element.location
      |> Error.raise_warning;

      `Styled (`Emphasis, non_link_inline_elements status ~surrounding content)
      |> Location.same element

and non_link_inline_elements status ~surrounding elements =
  List.map (non_link_inline_element status ~surrounding) elements

let rec inline_element :
    status ->
    Odoc_parser.Ast.inline_element with_location ->
    Comment.inline_element with_location =
 fun status element ->
  match element with
  | { value = #ast_leaf_inline_element; _ } as element ->
      (leaf_inline_element element :> Comment.inline_element with_location)
  | { value = `Styled (style, content); location } ->
      `Styled (style, inline_elements status content) |> Location.at location
  | { value = `Reference (kind, target, content) as value; location } -> (
      let { Location.value = target; location = target_location } = target in
      match Error.raise_warnings (Reference.parse target_location target) with
      | Result.Ok target ->
          let content =
            non_link_inline_elements status ~surrounding:value content
          in
          Location.at location (`Reference (target, content))
      | Result.Error error ->
          Error.raise_warning error;
          let placeholder =
            match kind with
            | `Simple -> `Code_span target
            | `With_text -> `Styled (`Emphasis, content)
          in
          inline_element status (Location.at location placeholder))
  | { value = `Link (target, content) as value; location } ->
      `Link (target, non_link_inline_elements status ~surrounding:value content)
      |> Location.at location

and inline_elements status elements = List.map (inline_element status) elements

let rec nestable_block_element :
    status ->
    Odoc_parser.Ast.nestable_block_element with_location ->
    Comment.nestable_block_element with_location =
 fun status element ->
  match element with
  | { value = `Paragraph content; location } ->
      Location.at location (`Paragraph (inline_elements status content))
  | { value = `Code_block { meta; delimiter = _; content; output }; location }
    ->
      let lang_tag =
        match meta with
        | Some { language = { Location.value; _ }; _ } -> Some value
        | None -> None
      in
      let outputs =
        match output with
        | None -> None
        | Some l -> Some (List.map (nestable_block_element status) l)
      in
      Location.at location (`Code_block (lang_tag, content, outputs))
  | { value = `Math_block s; location } -> Location.at location (`Math_block s)
  | { value = `Verbatim _; _ } as element -> element
  | { value = `Modules modules; location } ->
      let modules =
        List.fold_left
          (fun acc { Location.value; location } ->
            match
              Error.raise_warnings (Reference.read_mod_longident location value)
            with
            | Result.Ok r ->
                { Comment.module_reference = r; module_synopsis = None } :: acc
            | Result.Error error ->
                Error.raise_warning error;
                acc)
          [] modules
        |> List.rev
      in
      Location.at location (`Modules modules)
  | { value = `List (kind, _syntax, items); location } ->
      `List (kind, List.map (nestable_block_elements status) items)
      |> Location.at location
  | { value = `Table ((grid, align), (`Heavy | `Light)); location } ->
      let data =
        List.map
          (List.map (fun (cell, cell_type) ->
               (nestable_block_elements status cell, cell_type)))
          grid
      in
      `Table { Comment.data; align } |> Location.at location

and nestable_block_elements status elements =
  List.map (nestable_block_element status) elements

let tag :
    location:Location.span ->
    status ->
    Ast.ocamldoc_tag ->
    ( Comment.block_element with_location,
      internal_tags_removed with_location )
    Result.result =
 fun ~location status tag ->
  if not status.tags_allowed then
    (* Trigger a warning but do not remove the tag. Avoid turning tags into
       text that would render the same. *)
    Error.raise_warning (tags_not_allowed location);
  let ok t = Result.Ok (Location.at location (`Tag t)) in
  match tag with
  | (`Author _ | `Since _ | `Version _) as tag -> ok tag
  | `Deprecated content ->
      ok (`Deprecated (nestable_block_elements status content))
  | `Param (name, content) ->
      ok (`Param (name, nestable_block_elements status content))
  | `Raise (name, content) -> (
      match Error.raise_warnings (Reference.parse location name) with
      (* TODO: location for just name *)
      | Result.Ok target ->
          ok
            (`Raise
              (`Reference (target, []), nestable_block_elements status content))
      | Result.Error error ->
          Error.raise_warning error;
          let placeholder = `Code_span name in
          ok (`Raise (placeholder, nestable_block_elements status content)))
  | `Return content -> ok (`Return (nestable_block_elements status content))
  | `See (kind, target, content) ->
      ok (`See (kind, target, nestable_block_elements status content))
  | `Before (version, content) ->
      ok (`Before (version, nestable_block_elements status content))

(* When the user does not give a section heading a label (anchor), we generate
   one from the text in the heading. This is the common case. This involves
   simply scanning the AST for words, lowercasing them, and joining them with
   hyphens.

   This must be done in the parser (i.e. early, not at HTML/other output
   generation time), so that the cross-referencer can see these anchors. *)
let generate_heading_label : Comment.inline_element with_location list -> string
    =
 fun content ->
  (* Code spans can contain spaces, so we need to replace them with hyphens. We
     also lowercase all the letters, for consistency with the rest of this
     procedure. *)
  let replace_spaces_with_hyphens_and_lowercase s =
    let result = Bytes.create (String.length s) in
    s
    |> String.iteri (fun index c ->
           let c =
             match c with
             | ' ' | '\t' | '\r' | '\n' -> '-'
             | _ -> Astring.Char.Ascii.lowercase c
           in
           Bytes.set result index c);
    Bytes.unsafe_to_string result
  in

  let strip_locs li = List.map (fun ele -> ele.Location.value) li in
  (* Perhaps this should be done using a [Buffer.t]; we can switch to that as
     needed. *)
  let rec scan_inline_elements anchor = function
    | [] -> anchor
    | element :: more ->
        let anchor =
          match (element : Comment.inline_element) with
          | `Space -> anchor ^ "-"
          | `Word w -> anchor ^ Astring.String.Ascii.lowercase w
          | `Code_span c | `Math_span c ->
              anchor ^ replace_spaces_with_hyphens_and_lowercase c
          | `Raw_markup _ ->
              (* TODO Perhaps having raw markup in a section heading should be an
                 error? *)
              anchor
          | `Styled (_, content) ->
              content |> strip_locs |> scan_inline_elements anchor
          | `Reference (_, content) ->
              content |> strip_locs
              |> List.map (fun (ele : Comment.non_link_inline_element) ->
                     (ele :> Comment.inline_element))
              |> scan_inline_elements anchor
          | `Link (_, content) ->
              content |> strip_locs
              |> List.map (fun (ele : Comment.non_link_inline_element) ->
                     (ele :> Comment.inline_element))
              |> scan_inline_elements anchor
        in
        scan_inline_elements anchor more
  in
  content |> List.map (fun ele -> ele.Location.value) |> scan_inline_elements ""

let section_heading :
    status ->
    top_heading_level:int option ->
    Location.span ->
    [ `Heading of _ ] ->
    int option * Comment.block_element with_location =
 fun status ~top_heading_level location heading ->
  let (`Heading (level, label, content)) = heading in

  let text = inline_elements status content in

  let heading_label_explicit, label =
    match label with
    | Some label -> (true, label)
    | None -> (false, generate_heading_label text)
  in
  let label =
    Paths.Identifier.Mk.label
      (status.parent_of_sections, Names.LabelName.make_std label)
  in

  let mk_heading heading_level =
    let attrs = { Comment.heading_level; heading_label_explicit } in
    let element = Location.at location (`Heading (attrs, label, text)) in
    let top_heading_level =
      match top_heading_level with None -> Some level | some -> some
    in
    (top_heading_level, element)
  in

  match (status.sections_allowed, level) with
  | `None, _any_level ->
      Error.raise_warning (headings_not_allowed location);
      let text = (text :> Comment.inline_element with_location list) in
      let element =
        Location.at location
          (`Paragraph [ Location.at location (`Styled (`Bold, text)) ])
      in
      (top_heading_level, element)
  | `No_titles, 0 ->
      Error.raise_warning (titles_not_allowed location);
      mk_heading `Title
  | _, level ->
      let level' =
        match level with
        | 0 -> `Title
        | 1 -> `Section
        | 2 -> `Subsection
        | 3 -> `Subsubsection
        | 4 -> `Paragraph
        | 5 -> `Subparagraph
        | _ ->
            Error.raise_warning (bad_heading_level level location);
            (* Implicitly promote to level-5. *)
            `Subparagraph
      in
      (match top_heading_level with
      | Some top_level
        when status.sections_allowed = `All && level <= top_level && level <= 5
        ->
          Error.raise_warning
            (heading_level_should_be_lower_than_top_level level top_level
               location)
      | _ -> ());
      mk_heading level'

let validate_first_page_heading status ast_element =
  match status.parent_of_sections.iv with
  | `Page (_, name) | `LeafPage (_, name) -> (
      match ast_element with
      | { Location.value = `Heading (_, _, _); _ } -> ()
      | _invalid_ast_element ->
          let filename = Names.PageName.to_string name ^ ".mld" in
          Error.raise_warning (page_heading_required filename))
  | _not_a_page -> ()

let top_level_block_elements status ast_elements =
  let rec traverse :
      top_heading_level:int option ->
      Comment.block_element with_location list ->
      internal_tags_removed with_location list ->
      Comment.block_element with_location list =
   fun ~top_heading_level comment_elements_acc ast_elements ->
    match ast_elements with
    | [] -> List.rev comment_elements_acc
    | ast_element :: ast_elements -> (
        (* The first [ast_element] in pages must be a title or section heading. *)
        if status.sections_allowed = `All && top_heading_level = None then
          validate_first_page_heading status ast_element;

        match ast_element with
        | { value = #Odoc_parser.Ast.nestable_block_element; _ } as element ->
            let element = nestable_block_element status element in
            let element = (element :> Comment.block_element with_location) in
            traverse ~top_heading_level
              (element :: comment_elements_acc)
              ast_elements
        | { value = `Tag the_tag; location } -> (
            match tag ~location status the_tag with
            | Result.Ok element ->
                traverse ~top_heading_level
                  (element :: comment_elements_acc)
                  ast_elements
            | Result.Error placeholder ->
                traverse ~top_heading_level comment_elements_acc
                  (placeholder :: ast_elements))
        | { value = `Heading _ as heading; _ } ->
            let top_heading_level, element =
              section_heading status ~top_heading_level
                ast_element.Location.location heading
            in
            traverse ~top_heading_level
              (element :: comment_elements_acc)
              ast_elements)
  in
  let top_heading_level =
    (* Non-page documents have a generated title. *)
    match status.parent_of_sections.iv with
    | `Page _ | `LeafPage _ -> None
    | _parent_with_generated_title -> Some 0
  in
  traverse ~top_heading_level [] ast_elements

let strip_internal_tags ast : internal_tags_removed with_location list * _ =
  let rec loop tags ast' = function
    | ({ Location.value = `Tag (#Ast.internal_tag as tag); _ } as wloc) :: tl
      -> (
        let next tag = loop ({ wloc with value = tag } :: tags) ast' tl in
        match tag with
        | (`Inline | `Open | `Closed | `Hidden) as tag -> next tag
        | `Canonical { Location.value = s; location = r_location } -> (
            match
              Error.raise_warnings (Reference.read_path_longident r_location s)
            with
            | Result.Ok path -> next (`Canonical path)
            | Result.Error e ->
                Error.raise_warning e;
                loop tags ast' tl))
    | ({
         value =
           `Tag #Ast.ocamldoc_tag | `Heading _ | #Ast.nestable_block_element;
         _;
       } as hd)
      :: tl ->
        loop tags (hd :: ast') tl
    | [] -> (List.rev ast', List.rev tags)
  in
  loop [] [] ast

(** Append alerts at the end of the comment. Tags are favoured in case of alerts of the same name. *)
let append_alerts_to_comment alerts
    (comment : Comment.block_element with_location list) =
  let alerts =
    List.filter
      (fun alert ->
        let (`Tag alert) = alert.Location_.value in
        List.for_all
          (fun elem ->
            match (elem.Location_.value, alert) with
            | `Tag (`Deprecated _), `Alert ("deprecated", _) -> false
            | _ -> true)
          comment)
      alerts
  in
  comment @ (alerts : alerts :> Comment.docs)

let ast_to_comment ~internal_tags ~sections_allowed ~tags_allowed
    ~parent_of_sections ast alerts =
  Error.catch_warnings (fun () ->
      let status = { sections_allowed; tags_allowed; parent_of_sections } in
      let ast, tags = strip_internal_tags ast in
      let elts =
        top_level_block_elements status ast |> append_alerts_to_comment alerts
      in
      (elts, handle_internal_tags tags internal_tags))

let parse_comment ~internal_tags ~sections_allowed ~tags_allowed
    ~containing_definition ~location ~text =
  Error.catch_warnings (fun () ->
      let ast =
        Odoc_parser.parse_comment ~location ~text |> Error.raise_parser_warnings
      in
      ast_to_comment ~internal_tags ~sections_allowed ~tags_allowed
        ~parent_of_sections:containing_definition ast []
      |> Error.raise_warnings)

let parse_reference text =
  let location =
    Location_.
      {
        file = "";
        start = { line = 0; column = 0 };
        end_ = { line = 0; column = String.length text };
      }
  in
  Reference.parse location text
