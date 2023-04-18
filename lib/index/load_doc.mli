module Make (Storage : Db.Storage.S) : sig
  module Db : Db.S with type writer = Storage.writer

  val clear : unit -> unit
  val run : odoc_directory:string -> string * string -> unit
end
