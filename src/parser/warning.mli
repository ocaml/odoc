type t = { location : Location.span; message : string }

val to_string : t -> string

val make :
  ?suggestion:string ->
  ('a, Format.formatter, unit, Location.span -> t) format4 ->
  'a
