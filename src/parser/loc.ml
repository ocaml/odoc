type point = { line : int; column : int }

let make_point : Lexing.position -> point =
 fun Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } ->
  { line = pos_lnum; column = pos_cnum - pos_bol }

type span = { file : string; start : point; end_ : point }
type +'a with_location = { location : span; value : 'a }

let at location value = { location; value }
let location { location; _ } = location
let value { value; _ } = value
let map f annotated = { annotated with value = f annotated.value }
let is predicate { value; _ } = predicate value
let same annotated value = { annotated with value }

let dummy_pos : point = { line = -1; column = -1 }

let of_position : ?filename:string -> Lexing.position * Lexing.position -> span
    =
 fun ?filename (start, end_) ->
  let start_point = make_point start and end_point = make_point end_ in
  {
    file = Option.value ~default:start.pos_fname filename;
    start = start_point;
    end_ = end_point;
  }

let extract :
    input:string ->
    start_pos:Lexing.position ->
    end_pos:Lexing.position ->
    string =
 fun ~input ~start_pos ~end_pos ->
  String.sub input start_pos.pos_cnum (end_pos.pos_cnum - start_pos.pos_cnum)

let fmt { file; start; end_ } =
  let { line = sline; column = scol } = start
  and { line = eline; column = ecol } = end_ in
  Printf.sprintf
    "file: %s\nstart: { line : %d, col : %d }\nend: { line: %d; col: %d }" file
    sline scol eline ecol

let with_start_location : span -> 'a with_location -> 'a with_location =
 fun { start; _ } self -> { self with location = { self.location with start } }

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

let delimited : 'a with_location -> 'b with_location -> span =
 fun { location = startpos; _ } { location = endpos; _ } ->
  { file = startpos.file; start = startpos.start; end_ = endpos.end_ }

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

let map_location : (span -> span) -> 'a with_location -> 'a with_location =
 fun f located -> { located with location = f located.location }

(* Utilities for offsetting the locations of various AST nodes to be more accurate *)

let _offset_inline_elt located =
  let f =
    match located.value with
    | `Word _ | `Space _ -> Fun.id
    | `Code_span _ -> nudge_start (-1)
    | `Raw_markup (_, _) | `Styled _ | `Math_span _ -> nudge_start (-3)
    | `Reference (`Simple, _, _) -> nudge_start (-2)
    | `Reference (`With_text, ref, _) ->
        let ref = value ref in
        nudge_start (-(String.length ref + 4))
    | `Link (_, []) -> nudge_start (-2)
    | `Link (link, _ :: _) -> nudge_start (-(String.length link + 3))
  in
  map_location f located

(*
and nestable_block_element =
  [ `Paragraph of inline_element with_location list
  | `Code_block of code_block
  | `Verbatim of string
  | `Modules of string with_location list
  | `List of
    list_kind * list_syntax * nestable_block_element with_location list list
  | `Table of table
  | `Math_block of string  (** @since 2.0.0 *)
  | `Media of reference_kind * media_href with_location * string * media
    (** @since 3.0.0 *) ]
*)

let _offset_block_elt located =
  let f =
    match located.value with
    | `Paragraph children ->
        let loc = span @@ List.map location children in
        Fun.const loc
    | `Verbatim _ -> nudge_start (-2)
    | `Modules _ -> nudge_start @@ -String.length "{!modules:"
    | `List (`Heavy, _, _) -> nudge_start (-4)
    | `Table (_, `Heavy) -> nudge_start @@ -String.length "{table"
    | `Math_block _ -> nudge_start @@ -String.length "{math"
    | _ -> Fun.id
  in
  map_location f located
