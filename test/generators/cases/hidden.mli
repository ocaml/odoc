(** This test cases exercise hidden attribute. *)

val foo : int
(** This is normal commented text. *)

val bar : int [@@hidden]
(** OMG! *)

module M :
sig
  val baz : int

end [@@hidden]

module N :
sig
  val quux : int

  [@@@hidden]

  val omg : int
end
