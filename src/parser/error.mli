type full_location_payload = { location : Location.span; message : string }

type t = full_location_payload

val make :
  ?suggestion:string ->
  ('a, Format.formatter, unit, Location.span -> t) format4 ->
  'a

val to_string : t -> string

type 'a with_warnings = { value : 'a; warnings : t list }

type warning_accumulator = t list ref

val accumulate_warnings : (warning_accumulator -> 'a) -> 'a with_warnings

val warning : warning_accumulator -> t -> unit
