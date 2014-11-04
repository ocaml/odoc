
type 'a printer

val build: (Xmlm.output -> 'a -> unit) -> 'a printer

val unit: 'a printer -> Xmlm.output -> 'a DocOckTypes.Unit.t -> unit

val file: 'a printer -> Xmlm.output -> 'a DocOckTypes.Unit.t -> unit
