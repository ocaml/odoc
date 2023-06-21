module Elt = Elt
module Types = Types
module Storage = Storage
module Suffix_tree = Suffix_tree

type t = Types.t =

  { db_types : Suffix_tree.With_occ.reader
  ; db_names : Suffix_tree.With_elts.reader
  }


val list_of_string : string -> char list

module type S = sig
  type writer

  val export : writer -> unit
  val store_type_paths : Elt.t -> string list list -> unit
  val store_word : string -> Elt.t -> unit
  val load_counter : int ref
end

module Make (Storage : Storage.S) : S with type writer = Storage.writer
