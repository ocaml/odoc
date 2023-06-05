type 'a db = 'a Types.t

module type S = sig
  type writer

  val open_out : string -> writer
  val save : db:writer -> Elt.t array db -> unit
  val close_out : writer -> unit
  val load : string -> Elt.t array db list
end
