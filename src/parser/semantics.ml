module Location = Model.Location_
module Error = Model.Error
module Comment = Model.Comment

type 'a with_location = 'a Location.with_location



type status = {
  permissive : bool;
  sections_allowed : Ast.sections_allowed;
  mutable warnings : Error.t list;
}

let warning status message =
  if status.permissive then
    status.warnings <- message::status.warnings
  else
    raise_notrace (Error.Conveyed_by_exception message)



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

      | `Heading (level, label, content) ->
        match status.sections_allowed, level with
        | `None, _ ->
          let message : Error.t =
            `With_full_location {
              location = ast_element.location;
              error = "sections not allowed in this comment";
            }
          in
          warning status message;
          let element =
            Location.same ast_element
              (`Paragraph [Location.same ast_element
                (`Styled (`Bold, content))])
          in
          traverse ~parsed_a_title (element::comment_elements_acc) ast_elements

        | `All, 1 ->
          if parsed_a_title then begin
            let message : Error.t =
              `With_full_location {
                location = ast_element.location;
                error = "only one title-level heading is allowed";
              }
            in
            raise_notrace (Error.Conveyed_by_exception message)
          end;
          let element =
            Location.same ast_element (`Heading (`Title, label, content)) in
          traverse
            ~parsed_a_title:true (element::comment_elements_acc) ast_elements

        | _ ->
          let level =
            match level with
            | 2 -> `Section
            | 3 -> `Subsection
            | 4 -> `Subsubsection
            | _ ->
              let message : Error.t =
                `With_full_location {
                  location = ast_element.location;
                  error =
                    Printf.sprintf
                      "'%i': bad section level (2-4 allowed)" level;
                }
              in
              warning status message;
              if level < 2 then
                `Section
              else
                `Subsubsection
          in
          let element =
            Location.same ast_element (`Heading (level, label, content)) in
          traverse ~parsed_a_title (element::comment_elements_acc) ast_elements
  in

  traverse ~parsed_a_title:false [] ast_elements



let ast_to_comment ~permissive ~sections_allowed ast =
  let status =
    {
      permissive;
      sections_allowed;
      warnings = [];
    }
  in

  let result =
    Error.catch_conveyed_by_exception (fun () ->
      top_level_block_elements status ast)
  in

  let warnings = List.rev status.warnings in

  {Error.result; warnings}
