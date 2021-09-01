include Odoc_parser.Loc

let set_end_as_offset_from_start offset span =
  { span with end_ = { span.start with column = span.start.column + offset } }

let point_in_string s offset point =
  let rec scan_string line column index =
    if index >= offset then (line, column)
    else if index >= String.length s then (line, column)
    else
      match s.[index] with
      | '\n' -> scan_string (line + 1) 0 (index + 1)
      | _ -> scan_string line (column + 1) (index + 1)
  in

  let line, column = scan_string 0 0 0 in

  { line = point.line + line; column = point.column + column }

(* Calling this repeatedly on the same string can be optimized, but there is no
   evidence yet that performance of this is a problem. *)
let in_string s ~offset ~length s_span =
  {
    s_span with
    start = point_in_string s offset s_span.start;
    end_ = point_in_string s (offset + length) s_span.start;
  }

let pp_span_start fmt s =
  Format.fprintf fmt "File \"%s\", line %d, character %d" s.file s.start.line
    s.start.column
