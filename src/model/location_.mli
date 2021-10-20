include module type of struct
  include Odoc_parser.Loc
end

val set_end_as_offset_from_start : int -> span -> span

val in_string : string -> offset:int -> length:int -> span -> span

val pp : Format.formatter -> span -> unit

val pp_span_start : Format.formatter -> span -> unit

val span_equal : span -> span -> bool
