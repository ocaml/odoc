open Common
module Elt = Elt
module Types = Types
module Storage = Storage
module Trie_compact = Trie_compact
module Trie_gen = Trie_gen
module Cache = Cache

type 'a t = 'a Types.t =
  { db_types : 'a Int.Map.t Trie_gen.t
  ; db_names : 'a Trie_gen.t
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
