module Location = Location_

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

let not_allowed :
    ?suggestion:string ->
    what:string ->
    in_what:string ->
    Location.span ->
    Error.t =
 fun ?suggestion ~what ~in_what ->
  Error.make ?suggestion "%s is not allowed in %s."
    (String.capitalize_ascii what)
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
  | `Raw_markup of string option * string ]

type status = {
  warnings : Error.warning_accumulator;
  sections_allowed : Odoc_parser.Ast.sections_allowed;
  parent_of_sections : Paths.Identifier.LabelParent.t;
}

let leaf_inline_element :
    status ->
    ast_leaf_inline_element with_location ->
    Comment.leaf_inline_element with_location =
 fun status element ->
  match element with
  | { value = `Word _ | `Code_span _; _ } as element -> element
  | { value = `Space _; _ } -> Location.same element `Space
  | { value = `Raw_markup (target, s); location } -> (
      match target with
      | Some invalid_target
        when String.trim invalid_target = ""
             || String.contains invalid_target '%'
             || String.contains invalid_target '}' ->
          Error.warning status.warnings
            (invalid_raw_markup_target invalid_target location);
          Location.same element (`Code_span s)
      | None ->
          Error.warning status.warnings
            (default_raw_markup_target_not_supported location);
          Location.same element (`Code_span s)
      | Some target -> Location.same element (`Raw_markup (target, s)) )

type surrounding =
  [ `Heading of
    int
    * string option
    * Odoc_parser.Ast.inline_element Location_.with_location list
  | `Link of
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
      ( leaf_inline_element status element
        :> Comment.non_link_inline_element with_location )
  | { value = `Styled (style, content); _ } ->
      `Styled (style, non_link_inline_elements status ~surrounding content)
      |> Location.same element
  | ( { value = `Reference (_, _, content); _ }
    | { value = `Link (_, content); _ } ) as element ->
      not_allowed
        ~what:(describe_element element.value)
        ~in_what:(describe_element surrounding)
        element.location
      |> Error.warning status.warnings;

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
      ( leaf_inline_element status element
        :> Comment.inline_element with_location )
  | { value = `Styled (style, content); location } ->
      `Styled (style, inline_elements status content) |> Location.at location
  | { value = `Reference (kind, target, content) as value; location } -> (
      let { Location.value = target; location = target_location } = target in
      match Reference.parse status.warnings target_location target with
      | Result.Ok target ->
          let content =
            non_link_inline_elements status ~surrounding:value content
          in
          Location.at location (`Reference (target, content))
      | Result.Error error ->
          Error.warning status.warnings error;
          let placeholder =
            match kind with
            | `Simple -> `Code_span target
            | `With_text -> `Styled (`Emphasis, content)
          in
          inline_element status (Location.at location placeholder) )
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
  | ({ value = `Code_block _; _ } | { value = `Verbatim _; _ }) as element ->
      element
  | { value = `Modules modules; location } ->
      let modules =
        List.fold_left
          (fun acc { Location.value; location } ->
            match
              Reference.read_mod_longident status.warnings location value
            with
            | Result.Ok r ->
                { Comment.module_reference = r; module_synopsis = None } :: acc
            | Result.Error error ->
                Error.warning status.warnings error;
                acc)
          [] modules
        |> List.rev
      in
      Location.at location (`Modules modules)
  | { value = `List (kind, _syntax, items); location } ->
      `List (kind, List.map (nestable_block_elements status) items)
      |> Location.at location

and nestable_block_elements status elements =
  List.map (nestable_block_element status) elements

let tag :
    location:Location.span ->
    status ->
    Odoc_parser.Ast.tag ->
    ( Comment.block_element with_location,
      Odoc_parser.Ast.block_element with_location )
    Result.result =
 fun ~location status tag ->
  let ok t = Result.Ok (Location.at location (`Tag t)) in
  match tag with
  | (`Author _ | `Since _ | `Version _ | `Inline | `Open | `Closed) as tag ->
      ok tag
  | `Canonical { value = s; location = r_location } -> (
      let path = Reference.read_path_longident r_location s in
      match path with
      | Result.Ok path -> ok (`Canonical path)
      | Result.Error e ->
          Error.warning status.warnings e;
          let placeholder = [ `Word "@canonical"; `Space " "; `Code_span s ] in
          let placeholder = List.map (Location.at location) placeholder in
          Error (Location.at location (`Paragraph placeholder)) )
  | `Deprecated content ->
      ok (`Deprecated (nestable_block_elements status content))
  | `Param (name, content) ->
      ok (`Param (name, nestable_block_elements status content))
  | `Raise (name, content) ->
      ok (`Raise (name, nestable_block_elements status content))
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
let generate_heading_label : Comment.link_content -> string =
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
             | _ -> Char.lowercase_ascii c
           in
           Bytes.set result index c);
    Bytes.unsafe_to_string result
  in

  (* Perhaps this should be done using a [Buffer.t]; we can switch to that as
     needed. *)
  let rec scan_inline_elements anchor = function
    | [] -> anchor
    | element :: more ->
        let anchor =
          match element.Location.value with
          | `Space -> anchor ^ "-"
          | `Word w -> anchor ^ String.lowercase_ascii w
          | `Code_span c -> anchor ^ replace_spaces_with_hyphens_and_lowercase c
          | `Raw_markup _ ->
              (* TODO Perhaps having raw markup in a section heading should be an
                 error? *)
              anchor
          | `Styled (_, content) -> scan_inline_elements anchor content
        in
        scan_inline_elements anchor more
  in
  scan_inline_elements "" content

let section_heading :
    status ->
    top_heading_level:int option ->
    Location.span ->
    [ `Heading of _ ] ->
    int option * Comment.block_element with_location =
 fun status ~top_heading_level location heading ->
  let (`Heading (level, label, content)) = heading in

  let content =
    non_link_inline_elements status
      ~surrounding:(heading :> surrounding)
      content
  in

  let label =
    match label with
    | Some label -> label
    | None -> generate_heading_label content
  in
  let label =
    `Label (status.parent_of_sections, Names.LabelName.make_std label)
  in

  match (status.sections_allowed, level) with
  | `None, _any_level ->
      Error.warning status.warnings (headings_not_allowed location);
      let content = (content :> Comment.inline_element with_location list) in
      let element =
        Location.at location
          (`Paragraph [ Location.at location (`Styled (`Bold, content)) ])
      in
      (top_heading_level, element)
  | `No_titles, 0 ->
      Error.warning status.warnings (titles_not_allowed location);
      let element = `Heading (`Title, label, content) in
      let element = Location.at location element in
      let top_heading_level =
        match top_heading_level with None -> Some level | some -> some
      in
      (top_heading_level, element)
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
            Error.warning status.warnings (bad_heading_level level location);
            (* Implicitly promote to level-5. *)
            `Subparagraph
      in
      ( match top_heading_level with
      | Some top_level
        when status.sections_allowed = `All && level <= top_level && level <= 5
        ->
          Error.warning status.warnings
            (heading_level_should_be_lower_than_top_level level top_level
               location)
      | _ -> () );
      let element = `Heading (level', label, content) in
      let element = Location.at location element in
      let top_heading_level =
        match top_heading_level with None -> Some level | some -> some
      in
      (top_heading_level, element)

let validate_first_page_heading status ast_element =
  match status.parent_of_sections with
  | `RootPage name | `Page (_, name) | `LeafPage (_, name) -> (
      match ast_element with
      | { Location.value = `Heading (_, _, _); _ } -> ()
      | _invalid_ast_element ->
          let filename = Names.PageName.to_string name ^ ".mld" in
          Error.warning status.warnings (page_heading_required filename) )
  | _not_a_page -> ()

let top_level_block_elements :
    status ->
    Odoc_parser.Ast.block_element with_location list ->
    Comment.block_element with_location list =
 fun status ast_elements ->
  let rec traverse :
      top_heading_level:int option ->
      Comment.block_element with_location list ->
      Odoc_parser.Ast.block_element with_location list ->
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
                  (placeholder :: ast_elements) )
        | { value = `Heading _ as heading; _ } ->
            let top_heading_level, element =
              section_heading status ~top_heading_level
                ast_element.Location.location heading
            in
            traverse ~top_heading_level
              (element :: comment_elements_acc)
              ast_elements )
  in
  let top_heading_level =
    (* Non-page documents have a generated title. *)
    match status.parent_of_sections with
    | `RootPage _ | `Page _ | `LeafPage _ -> None
    | _parent_with_generated_title -> Some 0
  in
  traverse ~top_heading_level [] ast_elements

let ast_to_comment warnings ~sections_allowed ~parent_of_sections ast =
  let status = { warnings; sections_allowed; parent_of_sections } in
  top_level_block_elements status ast

let parse_comment ~sections_allowed ~containing_definition ~location ~text =
  let ast = Odoc_parser.parse_comment ~location ~text in
  let comment =
    Error.accumulate_warnings (fun warnings ->
        ast_to_comment warnings ~sections_allowed
          ~parent_of_sections:containing_definition ast.Odoc_parser.Error.value)
  in
  {
    Error.value = comment.value;
    Error.warnings =
      (ast.Odoc_parser.Error.warnings |> List.map Error.t_of_parser_t)
      @ comment.Error.warnings;
  }

let parse_reference text =
  let location =
    Location_.
      {
        file = "";
        start = { line = 0; column = 0 };
        end_ = { line = 0; column = String.length text };
      }
  in
  let result =
    Error.accumulate_warnings (fun warnings ->
        Reference.parse warnings location text)
  in
  match result.Error.value with
  | Ok x -> Ok x
  | Error m -> Error (`Msg (Error.to_string m))
