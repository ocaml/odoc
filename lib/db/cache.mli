open Common

val clear : unit -> unit

module type Memo = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val memo : t -> t
end

module String : Memo with type t = string
module Char_list : Memo with type t = char list
module String_list : Memo with type t = string list
module String_list_list : Memo with type t = string list list
module Elt_array : Memo with type t = Elt.t array
module Elt_set_trie_gen : Memo with type t = Elt.Set.t Trie_gen.t
module Elt_set_occ_trie_gen : Memo with type t = Elt.Set.t Int.Map.t Trie_gen.t
module Elt_array_trie_gen : Memo with type t = Elt.t Array.t Trie_gen.t

module Elt_array_occ_trie_gen :
  Memo with type t = Elt.t Array.t Int.Map.t Trie_gen.t
