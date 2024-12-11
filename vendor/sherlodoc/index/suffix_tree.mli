module Buf : sig
  type t

  val make : unit -> t
end

type t

val make : Buf.t -> t
val add_suffixes : t -> string -> Db.Entry.t -> unit
val export : summarize:bool -> t -> Db.String_automata.t
