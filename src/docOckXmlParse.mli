
type 'a parser

val build : (Xmlm.input -> 'a) -> 'a parser

type 'a result =
  | Ok of 'a DocOckTypes.Unit.t
  | Error of Xmlm.pos option * Xmlm.pos * string

val unit : 'a parser -> Xmlm.input -> 'a result

val file : 'a parser -> Xmlm.input -> 'a result
