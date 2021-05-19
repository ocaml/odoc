(* This module is exposed, but via the signature declared in Octavius *)

type t = { location : Loc.span; message : string }

val to_string : t -> string

val make :
  ?suggestion:string ->
  ('a, Format.formatter, unit, Loc.span -> t) format4 ->
  'a
