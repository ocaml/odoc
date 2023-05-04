module Elt = Elt
module Types = Types
module Storage = Storage
module Trie = Trie

val list_of_string : string -> char list

module type S = sig
  type writer

  val optimize : unit -> unit
  val export : writer -> unit
  val store_all : Elt.Set.elt -> char list list -> unit
  val store_name : char list -> Elt.Set.elt -> unit
  val load_counter : int ref
end

module Make (Storage : Storage.S) : S with type writer = Storage.writer
