type t = { location : Location.span; message : string }

val make :
  ?suggestion:string ->
  ('a, Format.formatter, unit, Location.span -> t) format4 ->
  'a

type 'a with_warnings = { value : 'a; warnings : t list }

type warning_accumulator = t list ref

val accumulate_warnings : (warning_accumulator -> 'a) -> 'a with_warnings

val warning : warning_accumulator -> t -> unit
