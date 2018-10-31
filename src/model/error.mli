type t

type 'a with_warnings = {
  result : 'a;
  warnings : t list;
}

val make : string -> Location_.span -> t
val filename_only : string -> string -> t
val format : ('a, unit, string, Location_.span -> t) format4 -> 'a

val to_string : t -> string

val raise_exception : t -> _
val to_exception : ('a, t) Result.result -> 'a
val catch : (unit -> 'a) -> ('a, t) Result.result

val shed_warnings : 'a with_warnings -> 'a
