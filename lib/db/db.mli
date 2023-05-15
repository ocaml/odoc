module Elt = Elt
module Types = Types
module Storage = Storage
module Trie = Trie
module Caches = Caches

val list_of_string : string -> char list

module type S = sig
  type writer

  val optimize : unit -> unit
  val export : writer -> unit
  val store_type : Elt.t -> char list list -> unit
  val store_word : string -> Elt.t -> unit
  val load_counter : int ref
end

module Make (Storage : Storage.S) : S with type writer = Storage.writer
