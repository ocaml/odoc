(** This module provides a way to do memory-sharing after the fact, for 
    a nuumber a OCaml types. 
    Every sharable element inside a type is also shared.*)

open Common

val clear : unit -> unit
(** [clear ()] removes every value from the caches of every types. *)

(** A type [t] and its [memo] function. *)
module type Cached = sig
  type t

  val memo : t -> t
  (** [memo v] is [v] with the maximum amount of shared memory. As side effect 
      is to register [v] and its subvalues to be shared in the future. *)
end

module String_ : Cached with type t = string
module Char_list_ : Cached with type t = char list
module String_list_ : Cached with type t = string list
module String_list_list_ : Cached with type t = string list list
module Kind_ : Cached with type t = Elt.Kind.t
module Elt_array_ : Cached with type t = Elt.t array
module Elt_set_trie_ : Cached with type t = Elt.Set.t Trie.t
module Elt_set_occ_trie_ : Cached with type t = Elt.Set.t Int.Map.t Trie.t
module Elt_array_trie_ : Cached with type t = Elt.t Array.t Trie.t
module Elt_array_occ_trie_ : Cached with type t = Elt.t Array.t Int.Map.t Trie.t
