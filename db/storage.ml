type db = Db_typedef.t

module type S = sig
  type writer

  val open_out : string -> writer
  val save : db:writer -> db -> unit
  val close_out : writer -> unit
  val load : string -> db list
end
