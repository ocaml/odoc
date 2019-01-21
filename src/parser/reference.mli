module Error = Model.Error
module Location_ = Model.Location_
module Paths = Model.Paths

val parse :
  Error.warning_accumulator -> Location_.span -> string ->
    (Paths.Reference.t, Error.t) Result.result

val read_path_longident :
  Location_.span -> string ->
    (Paths.Path.Module.t, Error.t) Result.result

val read_mod_longident :
  Error.warning_accumulator -> Location_.span -> string ->
    (Paths.Reference.Module.t, Error.t) Result.result
