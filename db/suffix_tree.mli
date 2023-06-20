module type SET = sig
  type t
  type elt

  val of_list : elt list -> t
  val is_empty : t -> bool
end

module Make (S : SET) : sig
  type writer

  val make : unit -> writer
  val add_suffixes : writer -> string -> S.elt -> unit

  type reader

  val export : writer -> reader
  val find : reader -> string -> reader
  val to_sets : reader -> S.t list
end
