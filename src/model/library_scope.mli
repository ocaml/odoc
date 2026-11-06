(** The libraries a root name is looked up among. *)

type t = private
  | Scoped of string list
      (** Among these libraries alone. The list is never empty. *)
  | First of string list
      (** TEMPORARY, until dune writes [@canonical /lib/Name.X]. Among these
          libraries, or anywhere if no directory given with [-L] for them holds
          a unit of that name, as when they are not given at link. The list is
          never empty. *)
  | Any  (** Anywhere on the search path. *)

val any : t

val of_libraries : string list -> t
(** [of_libraries libs] is [Scoped libs], sorted and without repeats, or [Any]
    when [libs] is empty: a unit that recorded no libraries is not limited. *)

val first : string list -> t
(** TEMPORARY: [first libs] is [First libs], or [Any] when [libs] is empty. *)

val pp : Format.formatter -> t -> unit
(** For debugging. *)
