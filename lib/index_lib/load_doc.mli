module Make (Storage : Db.Storage.S) : sig
  module Db : Db.S with type writer = Storage.writer

  val clear : unit -> unit
  val run : index:Odoc_search.Entry.t list -> unit
end
