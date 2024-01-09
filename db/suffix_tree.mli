module Buf : sig
  type t

  val make : unit -> t
end

type t

val make : Buf.t -> t
val add_suffixes : t -> string -> Entry.t -> unit
val export : t -> String_automata.t
