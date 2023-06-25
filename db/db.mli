module Elt = Elt
module Types = Types
module Storage = Storage
module Suffix_tree = Suffix_tree
module Occ = Occ

type t = Types.t =
  { db_names : Suffix_tree.With_elts.reader
  ; db_types : Suffix_tree.With_occ.reader
  }

type writer

val make : unit -> writer
val export : writer -> t
val store_type_paths : writer -> Elt.t -> string list list -> unit
val store_word : writer -> string -> Elt.t -> unit
