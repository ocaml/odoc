(* The [Lexing] module keeps track of only byte offsets into the input. To get
   line/column locations, the lexer usually has to call [Lexing.new_line] on
   every newline character.

   However, to keep the odoc lexer simple, it doesn't do that. Instead, this
   function is given the input string, and it returns a function which converts
   absolute offsets into the input into line/byte offset within line pairs. *)
let make_offset_to_location_function
    : string -> (int -> Model.Location_.point) = fun s ->

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
            Model.Location_.line = line_number;
            column = absolute_offset - line_start_offset
          }
        else
          scan_to_last_newline prefix
    in
    scan_to_last_newline reversed_newlines



let parse_comment
    ~permissive ~sections_allowed ~containing_definition ~location ~text =

  (* Converts byte offsets into the comment to line, column pairs, which are
     relative to the start of the file that contains the comment. *)
  let offset_to_location =
    let offset_to_location_relative_to_start_of_comment =
      lazy (make_offset_to_location_function text) in

    let offset_to_location_relative_to_start_of_file offset =
      let in_comment =
        (Lazy.force offset_to_location_relative_to_start_of_comment) offset in

      let line_in_file = in_comment.line + location.Lexing.pos_lnum - 1 in
      let offset_in_line =
        if in_comment.line = 1 then
          in_comment.column + location.Lexing.pos_cnum - location.Lexing.pos_bol
        else
          in_comment.column
      in

      {Model.Location_.line = line_in_file; column = offset_in_line}
    in

    offset_to_location_relative_to_start_of_file
  in

  let token_stream =
    let lexbuf = Lexing.from_string text in
    let input : Lexer.input =
      {
        file = location.Lexing.pos_fname;
        offset_to_location;
        lexbuf;
      }
    in
    Stream.from (fun _token_index -> Some (Lexer.token input lexbuf))
  in

  match Syntax.parse token_stream with
  | Error error ->
    {Model.Error.result = Error error; warnings = []}
  | Ok ast ->
    Semantics.ast_to_comment
      ~permissive
      ~sections_allowed
      ~parent_of_sections:containing_definition
      ast



type sections_allowed = Ast.sections_allowed
