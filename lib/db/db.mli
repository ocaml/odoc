module Elt = Types.Elt
module Types = Types
module Storage = Storage
module Tchar = Types.Tchar
module Elt_set = Types.Elt_set
module String_list_map = Types.String_list_map

val list_of_string : string -> char list

module type S = sig
  type writer

  val optimize : unit -> unit
  val export : writer -> unit
  val store_all : Elt_set.elt -> String_list_map.key list -> unit
  val store_name : Tchar.M.key list -> Elt_set.elt -> unit
  val load_counter : int ref
end

module Make (Storage : Storage.S) : S with type writer = Storage.writer
