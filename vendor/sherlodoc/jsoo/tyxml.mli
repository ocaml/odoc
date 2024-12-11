(* smaller js bundle than the real TyXml *)
module Html : sig
  type t

  val string_of_list : t list -> string

  type attr

  val a_class : string list -> attr
  val code : a:attr list -> t list -> t
  val span : a:attr list -> t list -> t
  val div : a:attr list -> t list -> t
  val txt : string -> t

  module Unsafe : sig
    val data : string -> t
  end
end
