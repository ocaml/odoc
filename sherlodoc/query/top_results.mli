module Make (IO : Io.S) : sig
  module Seq : module type of Io.Seq (IO)

  val of_seq
    :  query:Dynamic_cost.query
    -> limit:int
    -> Db.Entry.t Seq.t
    -> Db.Entry.t list IO.t
end
