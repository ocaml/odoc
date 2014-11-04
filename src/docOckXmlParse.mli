
type 'a parser

val build : (Xmlm.input -> 'a) -> 'a parser

val unit : 'a parser -> Xmlm.input -> 'a DocOckTypes.Unit.t

val file : 'a parser -> Xmlm.input -> 'a DocOckTypes.Unit.t
