open Common
module Elt = Elt
module Types = Types
module Storage = Storage
module Trie = Trie
module Cache = Cache

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
