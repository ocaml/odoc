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



let parse ~containing_definition ~comment_text =
  let token_stream =
    let lexbuf = Lexing.from_string comment_text in
    Stream.from (fun _token_index -> Some (Lexer.token lexbuf))
  in

  try
    Ok (Comment.comment ~parent_of_sections:containing_definition ~token_stream)

  with Helpers.Parse_error {start_offset; end_offset; text} ->
    let file =
      let root =
        Model.Paths.Identifier.label_parent_root containing_definition in
      Model.Root.Odoc_file.name root.file
    in
    let offset_to_location = make_offset_to_location_function comment_text in
    Error
      (`With_location {
        Model.Error.file;
        location = {
          start = offset_to_location start_offset;
          end_ = offset_to_location end_offset;
        };
        error = text;
      })
