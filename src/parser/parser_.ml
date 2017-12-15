(* The [Lexing] module keeps track of only byte offsets into the input. To get
   line/column locations, the lexer usually has to call [Lexing.new_line] on
   every newline character.

   However, to keep the odoc lexer simple, it doesn't do that. Instead, this
   function is given the input string, and it returns a function which converts
   absolute offsets into the input into line/byte offset within line pairs. *)
let make_offset_to_location_function
    : string -> (int -> Model.Error.location) = fun s ->

  let rec find_newlines line_number input_index newlines_accumulator =
    if input_index >= String.length s then
      newlines_accumulator
    else
      if s.[input_index] = '\n' then
        find_newlines
          (line_number + 1) (input_index + 1)
          ((line_number + 1, input_index + 1)::newlines_accumulator)
      else
        find_newlines line_number (input_index + 1) newlines_accumulator
  in

  let reversed_newlines : (int * int) list =
    find_newlines 1 0 [(1, 0)] in

  fun absolute_offset ->
    let rec scan_to_last_newline reversed_newlines_prefix =
      match reversed_newlines_prefix with
      | [] ->
        assert false
      | (line_number, line_start_offset)::prefix ->
        if line_start_offset <= absolute_offset then
          {
            Model.Error.line = line_number;
            column = absolute_offset - line_start_offset
          }
        else
          scan_to_last_newline prefix
    in
    scan_to_last_newline reversed_newlines



let parse_comment ~containing_definition ~location ~text:comment_text =
  let token_stream =
    let lexbuf = Lexing.from_string comment_text in
    Stream.from (fun _token_index -> Some (Lexer.token lexbuf))
  in

  try
    Ok (Comment.comment ~parent_of_sections:containing_definition ~token_stream)

  with Helpers.Parse_error {start_offset; end_offset; text = error_text} ->
    let file =
      let root =
        Model.Paths.Identifier.label_parent_root containing_definition in
      Model.Root.Odoc_file.name root.file
    in
    let offset_to_location = make_offset_to_location_function comment_text in
    let offset_to_location offset =
      let in_comment = offset_to_location offset in
      let line_in_file = in_comment.line + location.Lexing.pos_lnum - 1 in
      let offset_in_line =
        if in_comment.line = 1 then
          in_comment.column + location.Lexing.pos_cnum - location.Lexing.pos_bol
        else
          in_comment.column
      in
      {Model.Error.line = line_in_file; column = offset_in_line}
    in
    Error
      (`With_location {
        Model.Error.file;
        location = {
          start = offset_to_location start_offset;
          end_ = offset_to_location end_offset;
        };
        error = error_text;
      })
