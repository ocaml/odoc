type media = Reference of string | Link of string
type media_target = Audio | Video | Image

type tag =
  | Author of string
  | Deprecated
  | Param of string
  | Raise of string
  | Return
  | See of [ `Url | `File | `Document ] * string
  | Since of string
  | Before of string
  | Version of string
  | Canonical of string
  | Inline
  | Open
  | Closed
  | Hidden

let point_of_position Lexing.{ pos_lnum; pos_cnum; _ } =
  Loc.{ line = pos_lnum; column = pos_cnum }

type lexspan = Lexing.position * Lexing.position
let to_location : lexspan -> Loc.span =
 fun (start, end_) ->
  let open Loc in
  let start_point = point_of_position start
  and end_point = point_of_position end_ in
  { file = start.pos_fname; start = start_point; end_ = end_point }
