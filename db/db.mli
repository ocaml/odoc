module Entry = Entry
module Storage = Storage
module Suffix_tree = Suffix_tree
module Occ = Occ
module Type_polarity = Type_polarity
module Typexpr = Typexpr

type t = Db_typedef.t =
  { db_names : Suffix_tree.With_elts.reader
  ; db_types : Suffix_tree.With_occ.reader
  }
(** The type of a database.
    [db_names] is for text-based part of the query and [db_types] for the
    type-based part.
    [db_types] has [Entry.t array Int_map.t] ([Occ.t]) as a payload because we want
    the query [blabla : int -> int -> _] to return only entries that take at
    least two ints as arguments, an entry of type [int -> string] is invalid.
    The int_map map a number of occurences to a set of entries.
      *)

type writer

val make : unit -> writer
val export : writer -> t
val store_type_polarities : writer -> Entry.t -> Type_polarity.t list -> unit
val store_word : writer -> string -> Entry.t -> unit
