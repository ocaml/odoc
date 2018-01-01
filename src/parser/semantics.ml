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
    raise_notrace (Error.Conveyed_by_exception message)



let filter_section_heading status ~parsed_a_title location heading =
  let `Heading (level, label, content) = heading in

  let label =
    match label with
    | None ->
      None
    | Some label ->
      Some (Model.Paths.Identifier.Label (status.parent_of_sections, label))
  in

  match status.sections_allowed, level with
  | `None, _ ->
    let message : Error.t =
      `With_full_location {
        location;
        error = "sections not allowed in this comment";
      }
    in
    warning status message;
    let element =
      Location.at location
        (`Paragraph [Location.at location
          (`Styled (`Bold, content))])
    in
    parsed_a_title, element

  | `All, 1 ->
    if parsed_a_title then begin
      let message : Error.t =
        `With_full_location {
          location = location;
          error = "only one title-level heading is allowed";
        }
      in
      raise_notrace (Error.Conveyed_by_exception message)
    end;
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
        let message : Error.t =
          `With_full_location {
            location = location;
            error =
              Printf.sprintf "'%i': bad section level (2-4 allowed)" level;
          }
        in
        warning status message;
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

  let rec traverse ~parsed_a_title comment_elements_acc ast_elements =
    match ast_elements with
    | [] ->
      List.rev comment_elements_acc

    | ast_element::ast_elements ->
      match ast_element.Location.value with
      | #Comment.nestable_block_element
      | `Tag _ as element ->
        let element = Location.same ast_element element in
        traverse ~parsed_a_title (element::comment_elements_acc) ast_elements

      | `Heading _ as heading ->
        let parsed_a_title, element =
          filter_section_heading
            status ~parsed_a_title ast_element.Location.location heading
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

  let result =
    Error.catch_conveyed_by_exception (fun () ->
      top_level_block_elements status ast)
  in

  let warnings = List.rev status.warnings in

  {Error.result; warnings}
