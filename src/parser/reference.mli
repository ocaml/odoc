module Error = Model.Error
module Location_ = Model.Location_
module Paths = Model.Paths

val parse :
  Error.warning_accumulator -> Location_.span -> string ->
    (Paths.Reference.any, Error.t) Result.result

val read_path_longident :
  Location_.span -> string ->
    ([< Paths.Path.kind > `Module ] Paths.Path.t, Error.t) Result.result

val read_mod_longident :
  Error.warning_accumulator -> Location_.span -> string ->
    (Paths.Reference.module_, Error.t) Result.result
