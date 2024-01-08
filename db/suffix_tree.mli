module type SET = sig
  type t
  type elt

  val of_list : elt list -> t
  val is_empty : t -> bool
  val minimum : t -> elt option
  val equal_elt : elt -> elt -> bool
  val compare_elt : elt -> elt -> int
end

module Buf : sig
  type t

  val make : unit -> t
end

module Make (S : SET) : sig
  type writer
  (** A writer is an incomplete suffix tree.
      You can add suffixes to it. *)

  val make : Buf.t -> writer
  val add_suffixes : writer -> string -> S.elt -> unit

  type reader
  (** A reader is a completed suffix tree. You can make queries on it. Its size
      is smaller than the equivalent [writer]. *)

  val export : writer -> reader
  val find : reader -> string -> reader option
  val to_sets : reader -> S.t list

  val sets_tree
    :  union:('a -> 'a -> 'a)
    -> terminal:(S.t -> 'a)
    -> union_of_array:('a array -> 'a)
    -> reader
    -> 'a
end

module With_elts : module type of Make (Entry.Array)
(** [With_elts] is a suffix tree with array of entries at the leafs. It is used
    for the text-based part of the database. *)
