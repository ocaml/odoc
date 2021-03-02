include module type of struct include Odoc_parser.Location end

val set_end_as_offset_from_start : int -> span -> span

val in_string : string -> offset:int -> length:int -> span -> span
