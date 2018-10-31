type t

val make : string -> Location_.span -> t
val filename_only : string -> string -> t
val format : ('a, unit, string, Location_.span -> t) format4 -> 'a

val to_string : t -> string

val raise_exception : t -> _
val to_exception : ('a, t) Result.result -> 'a
val catch : (unit -> 'a) -> ('a, t) Result.result

type 'a with_warnings = {
  value : 'a;
  warnings : t list;
}

type warning_accumulator

val make_warning_accumulator : unit -> warning_accumulator
val warning : warning_accumulator -> t -> unit
val attach_accumulated_warnings : warning_accumulator -> 'a -> 'a with_warnings
val shed_warnings : 'a with_warnings -> 'a
