module Error = Odoc_model.Error
module Location_ = Odoc_model.Location_
module Paths = Odoc_model.Paths

type path = [ `Root of string | `Dot of Paths.Path.Module.t * string ]

val parse :
  Error.warning_accumulator ->
  Location_.span ->
  string ->
  (Paths.Reference.t, Error.t) Result.result

val read_path_longident :
  Location_.span -> string -> (path, Error.t) Result.result

val read_mod_longident :
  Error.warning_accumulator ->
  Location_.span ->
  string ->
  (Paths.Reference.Module.t, Error.t) Result.result
