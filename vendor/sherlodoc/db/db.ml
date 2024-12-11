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
