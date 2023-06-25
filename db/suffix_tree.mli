module type SET = sig
  type t
  type elt

  val of_list : elt list -> t
  val is_empty : t -> bool
  val equal_elt : elt -> elt -> bool
end

module Make (S : SET) : sig
  type writer
  (** A writer is an incomplete suffix tree.
      You can add suffixes to it. *)

  val make : unit -> writer
  val add_suffixes : writer -> string -> S.elt -> unit

  type reader
  (** A reader is a completed suffix tree. You can make queries on it.*)

  val export : writer -> reader
  val find : reader -> string -> reader option
  val to_sets : reader -> S.t list
end

module With_elts : module type of Make (Elt.Array)
module With_occ : module type of Make (Occ)
