module Entry = Entry
module Storage = Storage
module Type_polarity = Type_polarity
module Typexpr = Typexpr
module Occurences = Storage.Occurences
module String_automata = String_automata

type t = Storage.db =
  { db_names : String_automata.t
  ; db_pos_types : String_automata.t Occurences.t
  ; db_neg_types : String_automata.t Occurences.t
  }
(** The type of a search database.

    [db_names] is for text-based part of the query and [db_types] for the
    type-based part.

    [db_types] has [Entry.t array Int_map.t] ([Occ.t]) as a payload because we want
    the query [blabla : int -> int -> _] to return only entries that take at
    least two ints as arguments, an entry of type [int -> string] is invalid.
    The [Int_map.t] maps a number of occurences to a set of entries. See {!Occ}.
    [db_types] still is a suffix tree, so you can search in it only for text. The
    way we transform types into searchable text is in {!Type_polarity}. *)

type writer
(** The type that builds a database. You can use it to add things to it, but
    you cannot make queries on it. *)

val make : unit -> writer
(** [make ()] returns an empty search database. *)

val store_type_polarities : writer -> Entry.t -> Type_polarity.t Seq.t -> unit
val store_word : writer -> string -> Entry.t -> unit
val export : writer -> t
