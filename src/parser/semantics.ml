module Location = Model.Location_
module Error = Model.Error
module Comment = Model.Comment

type 'a with_location = 'a Location.with_location



type status = {
  permissive : bool;
  mutable warnings : Error.t list;
  sections_allowed : Ast.sections_allowed;
  parent_of_sections : Model.Paths.Identifier.label_parent;
}

let warning status message =
  if status.permissive then
    status.warnings <- message::status.warnings
  else
    Error.raise_exception message



(* TODO This and Token.describe probably belong in Parse_error. *)
let describe_element = function
  | `Reference (`Simple, _, _) ->
    Token.describe (`Simple_reference "")
  | `Reference (`With_text, _, _) ->
    Token.describe (`Begin_reference_with_replacement_text "")
  | `Link _ ->
    Token.describe (`Begin_link_with_replacement_text "")
  | `Heading (level, _, _) ->
    Token.describe (`Begin_section_heading (level, None))



let rec non_link_inline_element
    : surrounding:_ -> Ast.inline_element with_location ->
        Comment.non_link_inline_element with_location =
    fun ~surrounding element ->

  match element with
  | {value = #Comment.leaf_inline_element; _} as element ->
    element

  | {value = `Styled (style, content); _} ->
    `Styled (style, non_link_inline_elements ~surrounding content)
    |> Location.same element

  | {value = `Reference _; _}
  | {value = `Link _; _} as element ->
    Parse_error.not_allowed
      ~what:(describe_element element.value)
      ~in_what:(describe_element surrounding)
      element.location
    |> Error.raise_exception

and non_link_inline_elements ~surrounding elements =
  List.map (non_link_inline_element ~surrounding) elements

let rec inline_element
    : Ast.inline_element with_location -> Comment.inline_element with_location =
  function
  | {value = #Comment.leaf_inline_element; _} as element ->
    element

  | {value = `Styled (style, content); location} ->
    `Styled (style, inline_elements content)
    |> Location.at location

  | {value = `Reference (_, target, content) as value; location} ->
    `Reference (target, non_link_inline_elements ~surrounding:value content)
    |> Location.at location

  | {value = `Link (target, content) as value; location} ->
    `Link (target, non_link_inline_elements ~surrounding:value content)
    |> Location.at location

and inline_elements elements =
  List.map inline_element elements



let rec nestable_block_element
    : Ast.nestable_block_element with_location ->
        Comment.nestable_block_element with_location =
  function
  | {value = `Paragraph content; location} ->
    Location.at location (`Paragraph (inline_elements content))

  | {value = `Code_block _; _}
  | {value = `Verbatim _; _}
  | {value = `Modules _; _} as element ->
    element

  | {value = `List (kind, items); location} ->
    `List (kind, List.map nestable_block_elements items)
    |> Location.at location

and nestable_block_elements elements =
  List.map nestable_block_element elements



let tag : Ast.tag -> Comment.tag = function
  | `Author _
  | `Since _
  | `Version _
  | `Canonical _ as tag ->
    tag

  | `Deprecated content ->
    `Deprecated (nestable_block_elements content)

  | `Param (name, content) ->
    `Param (name, nestable_block_elements content)

  | `Raise (name, content) ->
    `Raise (name, nestable_block_elements content)

  | `Return content ->
    `Return (nestable_block_elements content)

  | `See (kind, target, content) ->
    `See (kind, target, nestable_block_elements content)

  | `Before (version, content) ->
    `Before (version, nestable_block_elements content)



let section_heading
    : status ->
      parsed_a_title:bool ->
      Location.span ->
      int ->
      string option ->
      (Ast.inline_element with_location) list ->
        bool * (Comment.block_element with_location) =
    fun status ~parsed_a_title location level label content ->

  let label =
    match label with
    | None ->
      None
    | Some label ->
      Some (Model.Paths.Identifier.Label (status.parent_of_sections, label))
  in

  let content =
    non_link_inline_elements
      ~surrounding:(`Heading (level, label, content)) content
  in

  match status.sections_allowed, level with
  | `None, _ ->
    warning status (Parse_error.sections_not_allowed location);
    let content = (content :> (Comment.inline_element with_location) list) in
    let element =
      Location.at location
        (`Paragraph [Location.at location
          (`Styled (`Bold, content))])
    in
    parsed_a_title, element

  | `All, 1 ->
    if parsed_a_title then
      Error.raise_exception (Parse_error.only_one_title_allowed location);
    let element = `Heading (`Title, label, content) in
    let element = Location.at location element in
    true, element

  | _ ->
    let level =
      match level with
      | 2 -> `Section
      | 3 -> `Subsection
      | 4 -> `Subsubsection
      | _ ->
        Parse_error.bad_section_level (string_of_int level) location
        |> warning status;
        if level < 2 then
          `Section
        else
          `Subsubsection
    in
    let element = `Heading (level, label, content) in
    let element = Location.at location element in
    parsed_a_title, element



let top_level_block_elements
    : status -> (Ast.block_element with_location) list ->
        (Comment.block_element with_location) list =
    fun status ast_elements ->

  let rec traverse
      : parsed_a_title:bool ->
        (Comment.block_element with_location) list ->
        (Ast.block_element with_location) list ->
          (Comment.block_element with_location) list =
      fun ~parsed_a_title comment_elements_acc ast_elements ->

    match ast_elements with
    | [] ->
      List.rev comment_elements_acc

    | ast_element::ast_elements ->
      match ast_element with
      | {value = #Ast.nestable_block_element; _} as element ->
        let element = nestable_block_element element in
        let element = (element :> Comment.block_element with_location) in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements

      | {value = `Tag the_tag; _} ->
        let element = Location.same ast_element (`Tag (tag the_tag)) in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements

      | {value = `Heading (level, label, content); _} ->
        let parsed_a_title, element =
          section_heading
            status
            ~parsed_a_title
            ast_element.Location.location
            level
            label
            content
        in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements
  in

  traverse ~parsed_a_title:false [] ast_elements



let ast_to_comment ~permissive ~sections_allowed ~parent_of_sections ast =
  let status =
    {
      permissive;
      warnings = [];
      sections_allowed;
      parent_of_sections;
    }
  in

  let result = Error.catch (fun () -> top_level_block_elements status ast) in
  let warnings = List.rev status.warnings in

  {Error.result; warnings}
