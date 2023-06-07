open Common
module Elt = Elt
module Types = Types
module Storage = Storage
module Trie = Trie
module Cache = Cache

val trie_with_array : Elt.Set.t Trie.t -> Elt.t array Trie.t
val trie_with_set : Elt.t array Trie.t -> Elt.Set.t Trie.t

val trie_with_array_occ :
  Elt.Set.t Int.Map.t Trie.t -> Elt.t array Int.Map.t Trie.t

val trie_with_set_occ :
  Elt.t array Int.Map.t Trie.t -> Elt.Set.t Int.Map.t Trie.t

type 'a t = 'a Types.t =
  { db_types : 'a Int.Map.t Trie.t
  ; db_names : 'a Trie.t
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
