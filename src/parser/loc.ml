type point = { line : int; column : int }
type span = { file : string; start : point; end_ : point }
type +'a with_location = { location : span; value : 'a }

let at location value = { location; value }
let location { location; _ } = location
let value { value; _ } = value
let map f annotated = { annotated with value = f annotated.value }
let is predicate { value; _ } = predicate value
let same annotated value = { annotated with value }

let of_position : ?filename:string -> Lexing.position * Lexing.position -> span
    =
 fun ?filename (start, end_) ->
  print_endline @@ "FILENAME: " ^ start.pos_fname;
  let to_point Lexing.{ pos_lnum; pos_cnum; _ } =
    { line = pos_lnum; column = pos_cnum }
  in
  let start_point = to_point start and end_point = to_point end_ in
  {
    file = Option.value ~default:start.pos_fname filename;
    start = start_point;
    end_ = end_point;
  }

let fmt { file; start; end_ } =
  let { line = sline; column = scol } = start
  and { line = eline; column = ecol } = end_ in
  Printf.sprintf
    "file: %s\nstart: { line : %d, col : %d }\nend: { line: %d; col: %d }" file
    sline scol eline ecol

let span spans =
  match spans with
  | [] ->
      {
        file = "_none_";
        start = { line = 1; column = 0 };
        end_ = { line = 1; column = 0 };
      }
  | first :: spans ->
      let last = List.fold_left (fun _ span -> span) first spans in
      { file = first.file; start = first.start; end_ = last.end_ }

let nudge_start offset span =
  { span with start = { span.start with column = span.start.column + offset } }

let nudge_end offset span =
  { span with end_ = { span.end_ with column = span.end_.column - offset } }

let nudge_map_start offset loc =
  { loc with location = nudge_start offset loc.location }

let nudge_map_end offset loc =
  { loc with location = nudge_end offset loc.location }

let spans_multiple_lines = function
  | {
      location =
        { start = { line = start_line; _ }; end_ = { line = end_line; _ }; _ };
      _;
    } ->
      end_line > start_line
